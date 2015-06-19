/* global d3 CoincidenceGraph */

var cGraph = new CoincidenceGraph("#d3graph");
// cGraph.loadCSV("data/wspolwyst_matury2014.csv");

d3.json("data/wspolwyst_matury2014.json", function (error, data) {
  cGraph.draw(data);
});
