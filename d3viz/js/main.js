/* global d3 CoincidenceGraph */

var cGraph = new CoincidenceGraph("#d3graph");
// cGraph.loadCSV("data/wspolwyst_matury2014.csv");

d3.json("data/wspolwyst_matury2014.json", function (error, data) {

  // dodawanie etykiet
  data.nodes.forEach(function (node) {
    if (node.nazwa.indexOf("j_") !== -1) {
      node.label = node.nazwa.slice(2, 5).toUpperCase();
    } else {
      node.label = node.nazwa.slice(0, 3).toUpperCase();
    }
  });

  cGraph.draw(data);
});
