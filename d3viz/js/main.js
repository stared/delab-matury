/* global d3 CoincidenceGraph */

var main = {};

var wybor = d3.select("body").append('select')
  .attr('id', 'koincydencje')
  .on('change', function (x, y) {
    main.cGraph.remove();
    toLoad[d3.select(this).node().value]();
  });

wybor.append('option')
  .attr('value', "matury2014")
  .text("matury2014");

wybor.append('option')
  .attr('value', "olimpiady")
  .text("olimpiady");

wybor.append('option')
  .attr('value', "demograficznie2014")
  .text("demograficznie2014");

var toLoad = {

  "matury2014": function () {
    d3.json("data/wspolwyst_matury2014.json", function (error, data) {

      // dodawanie etykiet
      data.nodes.forEach(function (node) {

        if (node.name.indexOf("j_") !== -1) {
          node.label = node.name.slice(2, 5).toUpperCase();
        } else {
          node.label = node.name.slice(0, 3).toUpperCase();
        }

        node.name = node.name.replace("j_", "j. ").replace("_", " ");

        if (node.name.indexOf("rozszerzona") !== -1) {
          node.category = "rozszerzona";
        } else {
          node.category = "podstawowa";
        }
        
      });

      main.cGraph = new CoincidenceGraph("#d3graph");
      main.cGraph.draw(data);
      main.cGraph.createLegend();

    });
  },

  "olimpiady": function () {
    d3.csv("data/olimpiady_koincydencje.csv", function (error, data) {
      
      var name;
      var nameTo;
      var nodes = [];
      var nodeIndex = {};
      var totalNumber = 1683683; // 2010 to 2014 (5 years)

      data.forEach(function (row) {
        name = row[""];
        nodeIndex[name] = nodes.length;
        nodes.push({name: name, count: +row[name]});
      });

      var links = [];
      data.forEach(function (row) {
        name = row[""];
        for (nameTo in row) {
          if (nameTo !== "" && name < nameTo) {
            links.push({
              source: nodeIndex[name],
              target: nodeIndex[nameTo],
              count:  +row[nameTo],
              oe:     (+row[nameTo] * totalNumber)/(nodes[nodeIndex[name]].count * nodes[nodeIndex[nameTo]].count)
            })
          }
        }
      });

      // dodawanie etykiet
      nodes.forEach(function (node) {

        if (node.name.indexOf("j. ") !== -1) {
          node.label = node.name.slice(3, 6).toUpperCase();
        } else if (node.name.indexOf("z ") !== -1) {
          node.label = node.name.slice(2, 5).toUpperCase();
        } else {
          node.label = node.name.slice(0, 3).toUpperCase();
        }

        node.name = node.name.replace("j_", "j. ").replace("_", " ");
      });

      main.cGraph = new CoincidenceGraph("#d3graph");
      main.cGraph.draw({nodes: nodes, links: links}, {maxSize: 50, baseCharge: -100, muteCategory: true});

    });
  },

  "demograficznie2014": function () {
    d3.json("data/koincydencje_demograficzne.json", function (error, data) {

      data.nodes.forEach(function (node) {
        node.label = node.name.slice(0, 4).toLowerCase();
      });

      main.cGraph = new CoincidenceGraph("#d3graph");
      main.cGraph.draw(data, {eoThresholdMin: 1.05, maxSize: 50, baseCharge: -100});
      main.cGraph.createLegend();

    });
  },

};

toLoad["matury2014"]();
