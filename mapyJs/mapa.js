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


//function miejsce_id (d) {
//  return d.poczta + ", woj. " + d.wojewodztwo;
//}

// do wyrzucenia
var ludzie_do_miast = function (ludzie) {
  var dict = {};
  var d, woj;
  for (var i = 0; i < ludzie.length; i++) {
    d = ludzie[i];
    woj = d.wojewodztwo;
    if (woj in dict) {
      dict[woj].osoby += 1;
    } else {
      dict[woj] = {osoby: 1, Longitude: +d.Longitude, Latitude: +d.Latitude};
    }
  }
  var res = [];
  for (var k in dict) {
    d = dict[k];
    res.push({woj: k, osoby: d.osoby, Longitude: d.Longitude, Latitude: d.Latitude})
  };
  return res.sort(function (a, b) {
    return b.osoby - a.osoby;
  });
}

// do wyrzucenia
var ludzie_do_miast_zliczenia = function (ludzie) {
  var dict = {};
  var d, woj;
  for (var i = 0; i < ludzie.length; i++) {
    d = ludzie[i];
    woj = d.wojewodztwo;
    if (woj in dict) {
      dict[woj] += 1;
    } else {
      dict[woj] = 1;
    }
  }
  return dict;
}

// do wyrzucenia
var ludzie_do_dziedzin = function (ludzie) {
  var dict = {};
  var d;
  for (var i = 0; i < ludzie.length; i++) {
    d = ludzie[i];
    dziedzina = d.dziedzina;
    if (dziedzina in dict) {
      dict[dziedzina].osoby += 1;
    } else {
      dict[dziedzina] = {osoby: 1};
    }
  }
  var res = [];
  for (var k in dict) {
    d = dict[k];
    res.push({dziedzina: k, osoby: d.osoby})
  };
  return res.sort(function (a, b) {
    return b.osoby - a.osoby;
  });
}

// do wyrzucenia
var ludzie_do_dziedzin_zliczenia = function (ludzie) {
  var dict = {};
  var d;
  for (var i = 0; i < ludzie.length; i++) {
    d = ludzie[i];
    dziedzina = d.dziedzina;
    if (dziedzina in dict) {
      dict[dziedzina] += 1;
    } else {
      dict[dziedzina] = 1;
    }
  }
  return dict;
}

function znajdz_nazwy_matur(matury_dane){
	var nazwy_matur = [];
	var nazwy_kolumn = Object.keys(matury_dane[0]);
	var re = /licz_(.*)/
	nazwy_kolumn.forEach(function(nazwa){
		var dopasowanie = re.exec(nazwa);
		if (dopasowanie != null) {
			nazwy_matur.push(dopasowanie[1]);
		};
	});
	return nazwy_matur;
};

function wyswietl_nazwe_matury (nazwa) {
	var przetworzona = nazwa.replace(/^j_/, "j. ")
		.replace(/_/, " ");
	return przetworzona;
};

function dane_o_maturze (matury_dane, przedmiot) {
	var wynik = {};
	matury_dane.forEach(function(wiersz){
		wynik[wiersz.wojewodztwo] = {
			Longitude: wiersz.Longitude,
			Latitude: wiersz.Latitude,
			srednia: wiersz["sr_" + przedmiot],
			zdajacy: wiersz["licz_" + przedmiot],
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



d3.json("poland_woj.topo.json", function(error_poland, poland_data) {
  d3.csv("kfnrd_miejsce_dziedzina.csv", function(error_dem, people_data) {
  	d3.csv("wojewodztwa.csv", function(error_mat, matury_data) {
//  		console.log(matury_data)
    	zacznij_wizualizajce(poland_data, people_data, matury_data);
    })
  })
});


function zacznij_wizualizajce (poland_data, people_data, matury_data) {

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
  lata.push("WSZYSTKIE"); // do zrobienia: wszystkie
  // na razie na sztywno
  


  var lata = svg.append("g").attr("id","lata").selectAll(".rok")
        .data(lata)

  lata.enter()
          .append("text")
            .attr("class", "rok")
            .on("mouseover", function (d) {
              lata
                .attr("y", function (c) {
                  return c == d ? 65 : 50; 
                })
                .classed("selected", function (c) {
                  return c == d;
                })
              odswiez_rok(people_data.filter(function (c) { return c.rok === d; }),
                 matury_data.filter(function (c) { return c.rok === d; }));
            })
            .attr("x", function (d, i) { return 30 + 45 * i; })
            .attr("y", function (d) {
              return d === "2014" ? 65 : 50; 
            })
            .text(function (d) { return d; });

  odswiez_rok(people_data.filter(function (d) { return d.rok === "2014"; }),
  	matury_data.filter(function (d) { return d.rok === "2014"; }));

}

function odswiez_rok (people_data_rok, matury_data_rok) {
  console.log(dane_maturzysci(matury_data_rok));
  console.log(ludzie_do_miast(people_data_rok));

  var kola = svg.selectAll('.kolo')
    .data(dane_maturzysci(matury_data_rok));//, function(d) {return d.wojewodztwo; });
    //.data(ludzie_do_miast(people_data_rok), function(d) {return d.woj; });

  kola.enter()
    .append("circle")
      .attr("class", "kolo")
      .attr("cx", function (d) { return projection([d.Longitude, d.Latitude])[0]; })
      .attr("cy", function (d) { return projection([d.Longitude, d.Latitude])[1]; })
      .attr("r", function (d) {console.log("r = " + Math.sqrt(d.zdajacy)/10); Math.sqrt(d.zdajacy)/10;})
      .append("title");

  kola
    .on("mouseover", function (d) {
      //wyswietl_dziedziny_w_miescie(dziedziny, people_data_rok, d);
      var pos = projection([d.Longitude, d.Latitude]);
      var r = 3 * Math.sqrt(d.zdajacy);
      console.log("r = " + r);
      tooltipShow(
        [d.zdajacy, "w", d.wojewodztwo].join(" "),
        pos[0] + 8 - r,
        pos[1] + 16 + r
      );
    })
    .on("mouseout", function (d) {
      //wyswietl_dziedziny_wszystkie(dziedziny);
      tooltipOut();
    });

  kola.exit()
    .transition().duration(CZAS_PRZEJSCIA)
      .attr("r", 0)
      .remove();

  wyswietl_wszystkie_kola(kola);

  // dziedziny do zamienienia w przedmioty
  var dziedziny = svg.selectAll('.dziedzina')
    .data(znajdz_nazwy_matur(matury_data_rok));//, function (d) { return d.dziedzina; });
    //.data(ludzie_do_dziedzin(people_data_rok), function (d) { return d.dziedzina; });

  var dziedziny_g = dziedziny.enter()
    .append("g")
      .attr("class", "dziedzina")
      .attr("transform", function (d, i) {
        return "translate(700," + (170 + 20 * i) + ")";
       });

  dziedziny_g.append("rect")
    .attr("x", -30)
    .attr("y", -15)
    .attr("width", 180)
    .attr("height", 20)
    .attr("fill", "white");

  dziedziny_g.append("text")
    .attr("class", "dziedzina_tekst")
    .attr("x", 15);


  dziedziny_g.append("text")
    .attr("class", "dziedzina_licznik")
    .attr("text-anchor", "end");

  dziedziny
    .on("mouseover", function (d) { wyswietl_miasta_w_dziedzinie(kola, people_data_rok, d); })
    .on("mouseout", function (d) { wyswietl_wszystkie_kola(kola); });

  dziedziny.exit()
    .remove();

  //wyswietl_dziedziny_wszystkie(dziedziny);
  wyswietl_matury_wszystkie (dziedziny);

  dziedziny.transition().duration(CZAS_PRZEJSCIA)
    .attr("transform", function (d, i) {
      return "translate(700," + (170 + 20 * i) + ")";
    });

}

// do zmiany na matury
function wyswietl_dziedziny_wszystkie (dziedziny) {

  dziedziny.select(".dziedzina_tekst") //.transition().duration(CZAS_PRZEJSCIA)
    .style("opacity", 1)
    .text(function (d) { return d.dziedzina; });

  dziedziny.select(".dziedzina_licznik") //.transition().duration(CZAS_PRZEJSCIA)
    .style("opacity", 1)
    .text(function (d) { return d.osoby; });

}

function wyswietl_matury_wszystkie (matury) {

  matury.select(".dziedzina_tekst") //.transition().duration(CZAS_PRZEJSCIA)
    .style("opacity", 1)
    .text(function (d) { return wyswietl_nazwe_matury(d); });

}


// do skasowania
//function wyswietl_dziedziny_w_miescie (dziedziny, people_data_rok, d) {

//  var czesc_ludzi = people_data_rok.filter(function (c) { return c.wojewodztwo == d.woj; });

//  var dziedziny_w_miescie = ludzie_do_dziedzin_zliczenia(czesc_ludzi);

//  dziedziny.select(".dziedzina_tekst") //.transition().duration(CZAS_PRZEJSCIA)
//    .style("opacity", function (d) {
//      return dziedziny_w_miescie[d.dziedzina] ? 1 : 0.4;
//    });

//  dziedziny.select(".dziedzina_licznik") //.transition().duration(CZAS_PRZEJSCIA)
//    .style("opacity", function (d) {
//      return dziedziny_w_miescie[d.dziedzina] ? 1 : 0.4;
//    })
//    .text(function (d) {
//      return dziedziny_w_miescie[d.dziedzina] || "";
//    });

//}


function wyswietl_wszystkie_kola (kola) {

  kola.transition()
    .duration(CZAS_PRZEJSCIA)
      .style("opacity", 0.5)
      .attr("r", function (d) { return Math.sqrt(d.zdajacy)/10; });

}


function wyswietl_miasta_w_dziedzinie (kola, people_data_rok, d) {

  var czesc_ludzi = people_data_rok.filter(function (c) { return c.dziedzina == d.dziedzina; });

  var miasta_w_dziedzinie = ludzie_do_miast_zliczenia(czesc_ludzi);

  kola.transition()
    .duration(CZAS_PRZEJSCIA)
      .style("opacity", 1)
      .attr("r", function (d) { return 3 * Math.sqrt(miasta_w_dziedzinie[d.woj] || 0); });

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
