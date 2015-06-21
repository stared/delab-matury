/* global d3 Tooltip */

function CoincidenceGraph(selector) {
  "use strict";

  var width = 700,
      height = 700;

  var svg = d3.select(selector).append("svg")
    .attr("width", width)
    .attr("height", height);

  var tooltip = new Tooltip(selector);

  this.loadCSV = function (path) {
    d3.csv(path, function (error, data) {
      return data; // TODO
    });
  };

  this.draw = function (graph) {

    graph.links = graph.links.sort(function (a, b) {
      return b.liczba - a.liczba;
    });

    graph.links = graph.links.filter(function (e) {
      return e.oe > 1.25; // || e.oe < 0.5;
    });

    var sizeScale = d3.scale.sqrt()
      .domain([0, d3.max(graph.nodes, function (d) { return d.liczba; })])
      .range([0, 75]);

    var maxOe = d3.max(graph.links, function (e) { return e.oe; });

    var colors = d3.scale.category10()
      .domain(d3.range(0, 10));

    var force = d3.layout.force()
        .charge(function (d) { return -75 * sizeScale(d.liczba); })
        .linkDistance(0)
        .gravity(0.5)
        .size([width, height])
        .linkStrength(function (e) {
          return e.oe > 1 ? (e.oe - 1) / (maxOe - 1) : 0;
        })
        .nodes(graph.nodes)
        .links(graph.links);

    var link = svg.selectAll(".link")
      .data(graph.links)  // sort it
      .enter().append("line")
        .attr("class", "link")
        .style("stroke-width", function (e) { return 2 * sizeScale(e.liczba); })
        .style("opacity", function (e) { return e.oe / maxOe; })
        .on("mouseover", function (e) {
          tooltip.show([e.source.nazwa, e.target.nazwa, e.liczba, e.oe].join("<br>"));
        })
        .on("mouseout", function () {
          tooltip.out();
        });

    var node = svg.selectAll(".node")
      .data(graph.nodes)
      .enter().append("circle")
        .attr("class", "node")
        .attr("r", function (d) { return sizeScale(d.liczba); })
        .style("fill", function (d) {
          return (d.nazwa.indexOf("rozszerzona") !== -1) ? colors(4) : colors(0);
        })
        .on("mouseover", function (d) {
          tooltip.show([d.nazwa, d.liczba].join("<br>"));
        })
        .on("mouseout", function () {
          tooltip.out();
        });

    var label = svg.selectAll(".label")
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

  };

}
