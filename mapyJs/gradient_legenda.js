//Na podstawie https://gist.github.com/nowherenearithaca/4449376

function rysuj_legende(dolny_kolor, kolor_srodkowy, gorny_kolor,min_wartosc, mean_wartosc, max_wartosc) {
  var szerokosc_kontenra = 100,
    wysokosc_kontenra = 300,
    x1 = 20,
    szerokosc_paska = 30,
    y1 = 100,
    wysokosc_paska = 200,
    liczba_odcieni = 35,
    liczba_kresek = 5;
    //czcionka = 10;
    
  var idGradient = "legendGradient";
	
  d3.select("svg #legenda").remove();
  
  var legenda = d3.select("svg").append("g")
  	.attr("id", "legenda")
    .attr("width", szerokosc_kontenra)
    .attr("height", wysokosc_kontenra);
   
  //pusta legenda
  legenda.append("g")
    .append("defs")
    .append("linearGradient")
      .attr("id",idGradient)
      .attr("x1","0%")
      .attr("x2","0%")
      .attr("y1","100%")
      .attr("y2","0%"); // x1=0, x2=100%, y1=y2 results in a horizontal gradient
                        // it would have been vertical if x1=x2, y1=0, y2=100%
                        // See
                        // http://www.w3.org/TR/SVG/pservers.html#LinearGradients
                        // for more details and fancier things you can do
                        //create the bar for the legend to go into
                        // the "fill" attribute hooks the gradient up to this rect
                        
  var srodek = wysokosc_paska*(max_wartosc - mean_wartosc)/(max_wartosc - min_wartosc) // srodkowa wartosc: odleglosc od koncow proporcjonalna do tej w domain.
                        
  var skala_osi = d3.scale.linear()
    .domain([min_wartosc, mean_wartosc, max_wartosc])
    .range([wysokosc_paska + y1, srodek + y1, y1]); 
    

  var os_y = d3.svg.axis().scale(skala_osi)
    .orient("right")
    .ticks(liczba_kresek) 
    .tickSize(25, 0, 0); // (odstep od liczb, )
    //.ticks();
    
  legenda.append("g")
    .attr("class", "axis") 
    .attr("transform", "translate(" + szerokosc_paska + ", 0)")
    .call(os_y);
    
  legenda.append("rect")
      .attr("fill","url(#" + idGradient + ")")
      .attr("x",x1)
      .attr("y",y1)
      .attr("width",szerokosc_paska)
      .attr("height",wysokosc_paska)
 
  var theData = [];
  var color = d3.scale.linear()  
      .domain([min_wartosc, mean_wartosc, max_wartosc])
      .range([dolny_kolor, srodkowy_kolor, gorny_kolor])
  
  var deltaValue = (max_wartosc - min_wartosc)/(liczba_odcieni - 1);
  var deltaPercent = 1/(liczba_odcieni-1);
  var value, valuecolor, opacity, p;

  for (var i=0;i < liczba_odcieni;i++) {
    value = min_wartosc + deltaValue*i;
    valuecolor = color(value);
    opacity = 1;
    p = 0 + deltaPercent*i;
    theData.push({"rgb":valuecolor, "opacity":opacity, "percent":p});
  };   
   
  var stops = d3.select('#' + idGradient).selectAll('stop')
      .data(theData);
    stops.enter().append('stop');
    stops.attr('offset',function(d) {
        return d.percent;
      })
      .attr('stop-color',function(d) {
        return d.rgb;
      })
      .attr('stop-opacity',function(d) {
        return d.opacity;
      });
      
};
