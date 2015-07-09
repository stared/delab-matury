/* global d3 Tooltip */

function CoincidenceGraph(selector) {
  "use strict";

  var width = 900,
      height = 700;

  var svg = d3.select(selector).append("svg")
    .attr("width", width)
    .attr("height", height);

  var g = svg.append("g");

  var tooltip = new Tooltip(selector);

  var siNumberApprox = function (x) {
    var prefix = d3.formatPrefix(x);
    var scaled = prefix.scale(x);
    return scaled.toFixed(scaled < 10 ? 1 : 0) + prefix.symbol;
  };

  this.loadCSV = function (path) {
    d3.csv(path, function (error, data) {
      return data; // TODO
    });
  };

  this.draw = function (graph) {

    graph.links = graph.links.sort(function (a, b) {
      return b.liczba - a.liczba;
    });

    // but it hides some data...
    graph.links = graph.links.filter(function (e) {
      return e.oe > 1.25; // || e.oe < 0.5;
    });

    var sizeScale = d3.scale.sqrt()
      .domain([0, d3.max(graph.nodes, function (d) { return d.liczba; })])
      .range([0, 75]);

    var maxOe = d3.max(graph.links, function (e) { return e.oe; });

    var opacityScale = d3.scale.log()
      .domain([1, maxOe])
      .range([0, 0.75]);

    var colors = d3.scale.category10()
      .domain(d3.range(0, 10));

    var force = d3.layout.force()
        .charge(function (d) { return -75 * sizeScale(d.liczba); })
        .linkDistance(0)
        .gravity(0.5)
        .size([width - 200, height])
        .linkStrength(function (e) {
          return e.oe > 1 ? (e.oe - 1) / (maxOe - 1) : 0;
        })
        .nodes(graph.nodes)
        .links(graph.links);

    var link = g.selectAll(".link")
      .data(graph.links)  // sort it
      .enter().append("line")
        .attr("class", "link")
        .style("stroke-width", function (e) { return 2 * sizeScale(e.liczba); })
        .style("opacity", function (e) { return opacityScale(e.oe); })
        .on("mouseover", function (e) {
          var text = siNumberApprox(e.liczba).replace("k", " tys.") + " zdajacych zarazem:<br>" +
                     e.source.nazwa + " i " + e.target.nazwa + "<br><br>" +
                     e.oe.toFixed(1) + "x bardziej prawdopodobna kombinacja niz losowo";
          tooltip.show(text);
        })
        .on("mouseout", function () {
          tooltip.out();
        });

    var node = g.selectAll(".node")
      .data(graph.nodes)
      .enter().append("circle")
        .attr("class", "node")
        .attr("r", function (d) { return sizeScale(d.liczba); })
        .style("fill", function (d) {
          return (d.nazwa.indexOf("rozszerzona") !== -1) ? colors(4) : colors(0);
        })
        .on("mouseover", function (d) {
          tooltip.show(siNumberApprox(d.liczba).replace("k", " tys.") + " zdajacych:<br>" + d.nazwa);
        })
        .on("mouseout", function () {
          tooltip.out();
        });

    var label = g.selectAll(".label")
      .data(graph.nodes)
      .enter().append("text")
        .attr("class", "label")
        .style("font-size", function (d) { return 0.7 * sizeScale(d.liczba); })
        .text(function (d) { return d.label; });

    var drag = force.drag();
    node.call(drag);

    force.start();

    force.on("tick", function() {
        node.attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; });

        label.attr("x", function(d) { return d.x; })
             .attr("y", function(d) { return d.y; });

        link.attr("x1", function(e) { return e.source.x; })
            .attr("y1", function(e) { return e.source.y; })
            .attr("x2", function(e) { return e.target.x; })
            .attr("y2", function(e) { return e.target.y; });
    });

    var legend = new Legend(selector + " svg");

    legend.g.attr("transform", "translate(650, 50)");

    legend.create([
      {name: "matura podstawowa", color: colors(0)},
      {name: "matura rozszerzona", color: colors(4)},
    ]);

  };

}
