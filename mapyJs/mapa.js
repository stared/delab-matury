var width = 960,
    height = 1160;

var projection = d3.geo.mercator()
    .center([20, 52])
    .scale(3000)
    .translate([400, 350]);

var path = d3.geo.path()
    .projection(projection);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var tooltip = d3.select("#tooltip");

var CZAS_PRZEJSCIA = 400;

var dolny_kolor =  "#FF0000"; //"red";
var gorny_kolor = "#0000FF"; //"blue";
var srodkowy_kolor = "#FFFFFF"; //"white";

var wybrana = {matura:"wszyscy maturzyści" , rok:"2014"};


// TODO
// wyswietlanie nazw matur w formacie: "przedmiot: podstawowa | rozszerzona"
// zachowanie matury po najechaniu na rok
// legenda
// pie chart - liczba zdajacych


function znajdz_nazwy_matur(matury_dane){

	var nazwy_kolumn = Object.keys(matury_dane[0]);
	var re = /licz_(.*)/
	
	var nazwy_matur = [];
	nazwy_kolumn.forEach(function(nazwa){
		var dopasowanie = re.exec(nazwa);
		if (dopasowanie != null) {
			nazwy_matur.push(dopasowanie[1]);
		};
	});
	
	nazwy_matur = nazwy_matur.sort(function(a,b){
		return a.toLowerCase().localeCompare(b.toLowerCase());
	});
	nazwy_matur.unshift("wszyscy maturzyści");
	return nazwy_matur;
};


function wyswietl_nazwe_matury (nazwa) {
	var przetworzona = nazwa.replace(/^j_/, "j. ")
		.replace(/_/, " ");
	return przetworzona;
};


function dane_o_maturze (matury_dane, matura) {

	var wynik = {};
	
	matury_dane.forEach(function(wiersz){
		wynik[wiersz.wojewodztwa] = {
			Longitude: wiersz.Longitude,
			Latitude: wiersz.Latitude,
			srednia: wiersz["sr_" + matura],
			zdajacy: wiersz["licz_" + matura],
		};
	});
	
	return wynik;
	
};


function dane_maturzysci (matury_dane) {
	var wynik = [];
	matury_dane.forEach(function(wiersz){
		wynik.push(
			{
			wojewodztwo: wiersz.wojewodztwa,
			Longitude: wiersz.Longitude,
			Latitude: wiersz.Latitude,
			zdajacy: wiersz["maturzysci"]
			}
		);
	});
	return wynik;
};


function kolory_skala (min_value, medium_value, max_value){

	var kolor = d3.scale.linear()  
    .domain([Number(min_value), Number(medium_value), Number(max_value)])
    .range([dolny_kolor, srodkowy_kolor, gorny_kolor]);
    
  return(kolor);

};
  
  
function srednia_krajowa (dane) {
	var sumy = [];
	var zdaj = [];
	for (var woj in dane){
		sumy.push(dane[woj].srednia*dane[woj].zdajacy);
		zdaj.push(dane[woj].zdajacy);
	};
	return d3.sum(sumy)/d3.sum(zdaj);
};



d3.json("poland_woj.topo.json", function(error_poland, poland_data) {
  d3.csv("wojewodztwa.csv", function(error_mat, matury_data) {
    zacznij_wizualizajce(poland_data, matury_data);
  })
});


function zacznij_wizualizajce (poland_data, matury_data) {

  svg.append("g").attr("id", "polska")
    .selectAll("path")
      .data(topojson.feature(poland_data, poland_data.objects.poland_woj).features)
      .enter()
        .append("path")
        .attr("id", function(d) { return d.id; })
          .attr("d", path)
          .attr("class", "wojewodztwo");

  var lata = [];
  for (var rok = 2010; rok <= 2014; rok++) lata.push(String(rok)); 
  lata.push("WSZYSTKIE"); 
  // na razie na sztywno
  


  var lata = svg.append("g").attr("id","lata").selectAll(".rok")
        .data(lata)

  lata.enter()
          .append("text")
            .attr("class", "rok")
            .on("click", function (d) {
            	wybrana.rok = d;
              lata
                .attr("y", function (c) {
                  return c == d ? 65 : 50; 
                })
                .classed("selected", function (c) {
                  return c == d;
                })
              odswiez_rok(matury_data.filter(function (c) { return c.rok === d; }));
            })
            .attr("x", function (d, i) { return 30 + 45 * i; })
            .attr("y", function (d) {
              return d === "2014" ? 65 : 50; 
            })
            .text(function (d) { return d; });

  odswiez_rok(matury_data.filter(function (d) { return d.rok === wybrana.rok; }));

}

function odswiez_rok (matury_data_rok) {

  var kola = svg.selectAll('.kolo')
    .data(dane_maturzysci(matury_data_rok));

  kola.enter()
    .append("circle")
      .attr("class", "kolo")
      .attr("cx", function (d) { return d.x = projection([d.Longitude, d.Latitude])[0]; })
      .attr("cy", function (d) { return d.y = projection([d.Longitude, d.Latitude])[1]; })
      .attr("r", function (d) { return d.r = Math.sqrt(d.zdajacy)/10;})
      .append("title");

  kola
    .on("mouseover", function (d) {
      var pos = projection([d.Longitude, d.Latitude]);
      tooltipShow(
        ["woj.", d.wojewodztwo,
         "<br>zdających:", Number(d.zdajacy).toLocaleString()].join(" "),
        d.x + d.r,
        d.y
      );
    })
    .on("mouseout", function (d) {
      tooltipOut();
    });

  kola.exit()
    .transition().duration(CZAS_PRZEJSCIA)
      .attr("r", 0)
      .remove();

	if (wybrana.matura === "wszyscy maturzyści")
		wyswietl_wszystkie_kola(kola);
	else
		wyswietl_kola_przedmioty(kola, matury_data_rok, wybrana.matura); 

  var matury = svg.selectAll('.matura')
    .data(znajdz_nazwy_matur(matury_data_rok));
    
  var matury_g = matury.enter()
    .append("g")
      .attr("class", "matura")
      .attr("transform", function (d, i) {
        return "translate(700," + (100 + 20 * i) + ")";
       });

  matury_g.append("rect")
    .attr("x", -30)
    .attr("y", -15)
    .attr("width", 180)
    .attr("height", 20)
    .attr("fill", "white");

  matury_g.append("text")
    .attr("class", "matura_tekst")
    .attr("x", function(d) {
    	console.log("d=" + d);
    	return d === wybrana.matura ? 0 : 15;
    });


  matury
    .on("click", function (d) { 
    	wybrana.matura = d;
    	matury.selectAll(".matura_tekst")
    		.attr("x", function (c) {
    	  	return c === d ? 0 : 15; 
      	});
			if (d=="wszyscy maturzyści")
				wyswietl_wszystkie_kola(kola);
			else
				wyswietl_kola_przedmioty(kola, matury_data_rok, d); 
    });


  matury.exit()
    .remove();
	
  wyswietl_matury(matury);

  matury.transition().duration(CZAS_PRZEJSCIA)
    .attr("transform", function (d, i) {
      return "translate(700," + (100 + 20 * i) + ")";
    });

}

function wyswietl_matury (matury) {

  matury.select(".matura_tekst")
    .style("opacity", 1)
    .text(function (d) { return wyswietl_nazwe_matury(d); });

}


function wyswietl_wszystkie_kola (kola) {

  kola.transition()
    .duration(CZAS_PRZEJSCIA)
      .style("opacity", 0.5)
      .style("fill", "#EFB701")
      .attr("r", function (d) { return Math.sqrt(d.zdajacy)/10; });
      
  kola
  .on("mouseover", function (d) {
    var pos = projection([d.Longitude, d.Latitude]);
    tooltipShow(
      ["woj.", d.wojewodztwo,
       "<br>zdających:", Number(d.zdajacy).toLocaleString()].join(" "),
      d.x + d.r,
      d.y
    );
  })
}


function wyswietl_kola_przedmioty (kola, matury_data_rok, przedmiot) {
	
	var wojewodztwa_przedmiot = dane_o_maturze (matury_data_rok, przedmiot);
	
	var srednie = [];
	for (var woj in wojewodztwa_przedmiot){
		srednie.push(wojewodztwa_przedmiot[woj].srednia);
	};
	 
	var kolory = kolory_skala(d3.min(srednie), srednia_krajowa(wojewodztwa_przedmiot), d3.max(srednie));
	
	kola.transition()
      .duration(CZAS_PRZEJSCIA)
        .style("opacity", 1)
        .style("stroke", "#000000")
        .style("fill", function(d) {return kolory(wojewodztwa_przedmiot[d.wojewodztwo].srednia)})
        .attr("r", function (d) {return Math.sqrt(wojewodztwa_przedmiot[d.wojewodztwo].zdajacy)/10; })

	kola.on("mouseover", function (d) {
		    	var pos = projection([d.Longitude, d.Latitude]);
		    	tooltipShow(
		      	["woj.", d.wojewodztwo,
		      	 "<br>zdających:", Number(wojewodztwa_przedmiot[d.wojewodztwo].zdajacy).toLocaleString(),
		      	 "<br>średni wynik:", Number(wojewodztwa_przedmiot[d.wojewodztwo].srednia).toPrecision(3), "%"].join(" "),
		      	d.x + d.r,
		      	d.y
		    	);
		  	});
        
}


function tooltipShow (html, x, y) {
  tooltip
    .style("display", "inline")
    .style("left", x + "px")
    .style("top", y + "px")
    .html(html);
}

function tooltipOut () {
  tooltip
    .style("display", "none");
}
