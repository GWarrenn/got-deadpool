
var battleSelector = d3.select('#battle')

load = 0
selectValue = ''

battleSelector
  .selectAll("option")
  .data(['Kings Landing','Winterfell'])
  .enter()
  .append("option")
	.attr("value", function (d) { return d; })
	.text(function (d) {
		return d[0].toUpperCase() + d.slice(1,d.length);
	})

	battleSelector.on('change',function() {

		var selectValue = d3.select(this)
			.property('value');

		displayBattleResults(selectValue);
		load = 1

	})

displayBattleResults('Kings Landing')

function displayBattleResults(value){

	if(value == "Winterfell"){
		file_1 = "https://raw.githubusercontent.com/GWarrenn/got-deadpool/master/data/predictions.csv"
		file_2 = "https://raw.githubusercontent.com/GWarrenn/got-deadpool/master/data/outcome_table.csv"
	} 

	else if(value == "Kings Landing"){
		file_1 = "https://raw.githubusercontent.com/GWarrenn/got-deadpool/master/data/predictions_westeros.csv"
		file_2 = "https://raw.githubusercontent.com/GWarrenn/got-deadpool/master/data/outcome_table_westoros.csv"
	} 

	updateAllCharts(file_1,file_2)
	//updateList(file_1)
}

function updateAllCharts(file_1,file_2){

	d3.csv(file_1, function(data){

		updateTable = function(data,filter_param) {

			document.getElementById("name").innerHTML = "Deadpool Predictions for: <b>" + filter_param + "</b>"

			d3.select("#character-predictions-table tbody").remove();
			d3.select("#character-predictions-table thead").remove();

			var table = d3.select('#character-predictions-table')
				.append('table')

			var thead = table.append('thead')
			var	tbody = table.append('tbody');

			data = _.orderBy(data, ['variable'], ['asc']);

			display_cols = ['Character','Prediction','Actual Fate','Ruling']
			columns = ['variable','value','actual','outcome']

			filtered_data = data.filter(function (a) { return a.sup_email == filter_param ; });		

			//// append the header row
			thead.append('tr')
			  .selectAll('th')
			  .data(display_cols).enter()
			  .append('th')
				.text(function (column) { return column; });

			// create a row for each object in the data
			var rows = tbody.selectAll('tr')
			  .data(filtered_data)
			  .enter()
			  .append('tr');

			rows.exit().remove();

			color = d3.scaleOrdinal()
			    .domain(["Incorrect","Correct"])
			    .range(["#ff9197","#90f98e"]);

			// create a cell in each row for each column
			cells = rows.selectAll('td')
				.data(function (row) {
					return columns.map(function (column) {
						return {column: column, value: row[column]};
					});
				})
				.enter()
				.append('td')
				.style("background-color", function(d){ if(d.column == "outcome") return color(d.value);})
				.text(function (d) { return d.value; });

			cells.exit().remove();

		}	

		var list = _.uniqBy(data, function (e) {
			return e.sup_email;
		});

		var names = _.map(list, 'sup_email').sort();

		var dropDown = d3.select('#nameDropdown')

		dropDown
		  .selectAll("option")
		  .data(names)
		  .enter()
		  .append("option")
			.attr("value", function (d) { return d; })
			.text(function (d) {
				return d[0].toUpperCase() + d.slice(1,d.length);
			})
		  .on("change", onchange)

		dropDown.on('change',function() {

			selectValue = d3.select(this)
				.property('value');

			updateTable(data,selectValue);

		})

		if(load == 0){
			temp_filter = names[0]
			updateTable(data,temp_filter)
		}
		else{
			updateTable(data,selectValue)
		}	

	});		

	d3.csv(file_2, function(data){

			document.getElementById("header").innerHTML = "Deadpool Rankings"

			var format = d3.format(",.2%")

			// format the data
			data.forEach(function(d) {
				d.rank = +d.rank
				d.Correct = +d.Correct
				d.Correct_fmt = format(+d.Correct)
				//d.Correct_fmt = +d.Correct_fmt
			});

			d3.select("#outcomes-table tbody").remove();
			d3.select("#outcomes-table thead").remove();

			var table = d3.select('#outcomes-table')
				.append('table')

			var thead = table.append('thead')
			var	tbody = table.append('tbody');

			data = _.orderBy(data, ['rank'], ['asc']);

			display_cols = ['Name','Percent Correct','Rank']
			columns = ['sup_email','Correct_fmt','rank']

			//// append the header row
			thead.append('tr')
			  .selectAll('th')
			  .data(display_cols).enter()
			  .append('th')
				.text(function (column) { return column; });

			// create a row for each object in the data
			var rows = tbody.selectAll('tr')
			  .data(data)
			  .enter()
			  .append('tr');

			rows.exit().remove();

			min = _.minBy(data, function(o) {
					return o.Correct_fmt;
			})

			max = _.maxBy(data, function(o) {
					return o.Correct_fmt;
			})

			color = d3.scaleLinear()
			    .domain([90,45])
			    .range(["#ff4500","#ffffff"]);

			back_to_number = d3.format(".4r")

			// create a cell in each row for each column
			cells = rows.selectAll('td')
				.data(function (row) {
					return columns.map(function (column) {
						return {column: column, value: row[column]};
					});
				})
				.enter()
				.append('td')
				.style("background-color", function(d){ if(d.column == "Correct_fmt") return color(+d.value.replace("%",""));})
				.text(function (d) { return d.value; });

			cells.exit().remove();	

	});		
}

function updateList(file_1){
	d3.csv(file_1, function(data){

		var list = _.uniqBy(data, function (e) {
			return e.sup_email;
		});

		var names = _.map(list, 'sup_email').sort();

		var dropDown = d3.select('#nameDropdown')

		dropDown
		  .selectAll("option")
		  .data(names)
		  .enter()
		  .append("option")
			.attr("value", function (d) { return d; })
			.text(function (d) {
				return d[0].toUpperCase() + d.slice(1,d.length);
			})
		  .on("change", onchange)

		dropDown.on('change',function() {

			var selectValue = d3.select(this)
				.property('value');

			updateTable(data,selectValue);

		})
	})
}