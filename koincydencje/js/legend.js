function Legend (selector) {

  var boxSize = 20;
  var spacing = 30;

  this.g = d3.select(selector).append('g');

  this.create = function (nameColorList) {

    var boxes = this.g.selectAll('.legend-box');
    var labels = this.g.selectAll('.legend-label');

    boxes.data(nameColorList)
      .enter()
      .append('rect')
        .attr('class', 'legend-box')
        .attr('x', 0)
        .attr('y', function (d, i) { return spacing * i; })
        .attr('width', boxSize)
        .attr('height', boxSize)
        .style('fill', function (d) { return d.color; });

    labels.data(nameColorList)
      .enter()
      .append('text')
        .attr('class', 'legend-label')
        .attr('x', spacing)
        .attr('y', function (d, i) { return spacing * i + 0.75 * boxSize; })
        .text(function (d) { return d.name; })
        .style("font-size", "" + boxSize + "px");

  };

}
