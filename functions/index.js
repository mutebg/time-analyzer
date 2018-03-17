const functions = require("firebase-functions");
const admin = require("firebase-admin");
const express = require("express");
const cors = require("cors");
const request = require("request-promise-native");
const _ = require("lodash");

const app = express();
app.use(cors());
app.enable("trust proxy");

admin.initializeApp(functions.config().firebase);

const url = ({ kind, dateFrom, dateTo, apiKey }) =>
  `https://www.rescuetime.com/anapi/data?key=${apiKey}&perspective=interval&restrict_kind=${kind}&interval=hour&restrict_begin=${dateFrom}&restrict_end=${dateTo}&format=json`;

app.get("/document/:date_from/:date_to", (req, res) => {
  const dateFrom = req.params.date_from;
  const dateTo = req.params.date_to;
  const apiKey = req.query.token;
  request
    .get(url({ kind: "document", dateFrom, dateTo, apiKey }))
    .then(response => JSON.parse(response))
    .then(response => {
      const { rows } = response;
      const list = rows; // rows.map(rowToObj);
      const groupedByHour = _.groupBy(list, "0");
      const hours = Object.keys(groupedByHour);

      const data = hours.map(hour => {
        const activitiesGrouped = _.groupBy(groupedByHour[hour], "3");
        const activitiesNames = Object.keys(activitiesGrouped);
        const activities = activitiesNames.map(actName => {
          const actList = activitiesGrouped[actName];
          const { totalTime, documents } = actList.reduce(
            (acc, curr) => {
              acc.totalTime += curr[1];
              acc.documents.push({
                TimeSpent: curr[1],
                Name: curr[4]
              });

              return acc;
            },
            {
              totalTime: 0,
              documents: []
            }
          );

          return {
            TotalTimeSpent: totalTime,
            Activity: actName,
            Documents: documents,
            Category: actList[0][5],
            Productivity: actList[0][6]
          };
        }, []);

        return {
          Date: hour,
          Activities: activities
        };
      });

      res.json(data);
    })
    .catch(err => {
      console.log(err);
    });
});

// Expose the API as a function
exports.api = functions.https.onRequest(app);
