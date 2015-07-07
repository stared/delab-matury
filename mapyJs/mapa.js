var width = 1100,
    height = 1160;

var projection = d3.geo.mercator()
    .center([20, 52])
    .scale(3000)
    .translate([430, 340]);

var path = d3.geo.path()
    .projection(projection);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var tooltip = d3.select("#tooltip");

var CZAS_PRZEJSCIA = 400;

var dolny_kolor =  "red";
		gorny_kolor = "blue";
		srodkowy_kolor = "white";
		
var mianownik = 4.6;

var wybrana = {matura:"wszyscy maturzyści" , rok:"2014"};


// TODO
// wyswietlanie nazw matur w formacie: "przedmiot: podstawowa | rozszerzona"
// legenda: liczba zdajacych
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


function dane_o_maturze (matury_dane, nazwa_matury) {

	var wynik = [];
	
	matury_dane.forEach(function(wiersz){
		wynik.push(
		 {
			wojewodztwo: wiersz.wojewodztwa,
			x: projection([wiersz.Longitude, wiersz.Latitude])[0],
			y: projection([wiersz.Longitude, wiersz.Latitude])[1],
			r: Math.sqrt(wiersz["licz_" + nazwa_matury])/mianownik,
			srednia: wiersz["sr_" + nazwa_matury],
			zdajacy: wiersz["licz_" + nazwa_matury]
		}
	)});
	
	return wynik;
	
}


function dane_maturzysci (matury_dane) {
	var wynik = [];
	matury_dane.forEach(function(wiersz){
		wynik.push(
			{
			wojewodztwo: wiersz.wojewodztwa,
			x: projection([wiersz.Longitude, wiersz.Latitude])[0],
			y: projection([wiersz.Longitude, wiersz.Latitude])[1],
			r: Math.sqrt(wiersz["maturzysci"])/mianownik,
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
	dane.forEach(function(woj){
		sumy.push(woj.srednia*woj.zdajacy);
		zdaj.push(woj.zdajacy);
	});
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
  lata.push("ŚREDNIA WIELOLETNIA"); 
  // na razie na sztywno
  


  var lata = svg.append("g").attr("id","lata").selectAll(".rok")
        .data(lata)

  lata.enter()
          .append("text")
            .attr("class", "rok")
            .classed("selected", function (c) {
                  return c == wybrana.rok;
             })
            .on("click", function (d) {
            	wybrana.rok = d;
            	odswiez_rok(matury_data, kola);
              lata
                .attr("y", function (c) {
                  return c == d ? 55 : 40; 
                })
                .classed("selected", function (c) {
                  return c == d;
                })
            })
            .attr("x", function (d, i) { return 170 + 45 * i; })
            .attr("y", function (d) {
              return d === "2014" ? 55 : 40; 
            })
            .text(function (d) { return d; });
  
  // wartość startowa          
  var matury_data_rok = matury_data.filter(function (d) { return d.rok === wybrana.rok; });
            
  var kola = svg.selectAll('.kolo')
  	.data(dane_maturzysci(matury_data_rok));

  kola.enter()
    .append("circle")
      .attr("class", "kolo")
      .attr("cx", function (d) { return d.x; })
      .attr("cy", function (d) { return d.y; })
      .attr("r", 0) 
      .append("title");
      
      
  kola
		.on("mouseover", function (d) {
		  tooltipShow(
		    ["woj.", d.wojewodztwo,
		     "<br>zdających:", Math.round(Number(d.zdajacy)).toLocaleString()].join(" "),
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
    	return d === wybrana.matura ? 0 : 15;
    })
    .classed("selected", function (c) {
      return c === wybrana.matura;
    });


  matury
    .on("click", function (d) { 
    	wybrana.matura = d;
    	matury.selectAll(".matura_tekst")
    		.attr("x", function (c) {
    	  	return c === d ? 0 : 15; 
      	})
      	.classed("selected", function (c) {
          return c == d;
        });
			odswiez_rok (matury_data, kola); 
    });


  matury.exit()
    .remove();
	
  wyswietl_matury(matury);

  matury.transition().duration(CZAS_PRZEJSCIA)
    .attr("transform", function (d, i) {
      return "translate(700," + (100 + 20 * i) + ")";
    });

  odswiez_rok(matury_data, kola);

}

function odswiez_rok (matury_data, kola) {

	var matury_data_rok = matury_data.filter(function (c) {return c.rok === wybrana.rok; })

	if (wybrana.matura === "wszyscy maturzyści")
		wyswietl_wszystkie_kola(kola, matury_data_rok);
	else
		wyswietl_kola_przedmioty(kola, matury_data_rok); 

}

function wyswietl_matury (matury) {

  matury.select(".matura_tekst")
    .style("opacity", 1)
    .text(function (d) { return wyswietl_nazwe_matury(d); });

}


function wyswietl_wszystkie_kola (kola, matury_data_rok) {

	kola
    .data(dane_maturzysci(matury_data_rok));

  kola.transition()
    .duration(CZAS_PRZEJSCIA)
      .style("opacity", 0.5)
      .style("fill", "#EFB701")
      .attr("r", function (d) { return d.r; });
      
  kola
  .on("mouseover", function (d) {
    tooltipShow(
      ["woj.", d.wojewodztwo,
       "<br>zdających:", Math.round(Number(d.zdajacy)).toLocaleString()].join(" "),
      d.x + d.r,
      d.y
    );
  });
  
  // usuwa legendę dla kolorów
  d3.select("svg #legenda").remove();
  
}


function wyswietl_kola_przedmioty (kola, matury_data_rok) {
	
	var wojewodztwa_przedmiot = dane_o_maturze (matury_data_rok, wybrana.matura);
	
	var srednie = wojewodztwa_przedmiot.map (function(woj){
		if (!isNaN(woj.srednia)){
			return woj.srednia
		};
	});

	var minW = Number(d3.min(srednie)),
			meanW = Number(srednia_krajowa(wojewodztwa_przedmiot)),
			maxW = Number(d3.max(srednie));
	 
	var kolory = kolory_skala(minW, meanW, maxW);
	
	kola
    .data(wojewodztwa_przedmiot);
	
	kola.transition()
      .duration(CZAS_PRZEJSCIA)
        .style("opacity", 1)
        .style("stroke", "#000000")
        .style("fill", function(d) { return kolory(Number(d.srednia))})
        .attr("r", function (d) { return d.r; })

	kola.on("mouseover", function (d) {
		    	tooltipShow(
		      	["woj.", d.wojewodztwo,
		      	 "<br>zdających:", Math.round(Number(d.zdajacy)).toLocaleString(),
		      	 "<br>średni wynik:", Number(d.srednia).toPrecision(3), "%"].join(" "),
		      	d.x + d.r,
		      	d.y
		    	);
		  	});
		  	
	rysuj_legende(dolny_kolor, srodkowy_kolor, gorny_kolor, minW, meanW, maxW);
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
