d3.csv("C:/Users/augus/Desktop/outcome_table.csv", function(data){

	list = _.uniqBy(data, function (e) {
		return e.email;
	});

	var names = _.map(list, 'Name');

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

	var updateTable = function(data,filter_param) {

		document.getElementById("name").innerHTML = "Deadpool Predictions for: <b>" + filter_param + "</b>"

		d3.select("#character-predictions-table tbody").remove();
		d3.select("#character-predictions-table thead").remove();

		var table = d3.select('#character-predictions-table')
			.append('table')

		var thead = table.append('thead')
		var	tbody = table.append('tbody');

		data = _.orderBy(data, ['variable'], ['asc']);

		display_cols = ['Character','Prediction','Actual Fate']
		columns = ['variable','value','actual']

		filtered_data = data.filter(function (a) { return a.email == filter_param ; });		

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

		eliminated = filtered_data.filter(function (a) { return a.eliminated == "1" ; });		

		var color = d3.scaleOrdinal()
		    .domain(eliminated)
		    .range("#FF0000", "#FF0000");

		// create a cell in each row for each column
		cells = rows.selectAll('td')
			.data(function (row) {
				return columns.map(function (column) {
					return {column: column, value: row[column]};
				});
			})
			.enter()
			.append('td')
			//.style("background-color", function(d){ if(d.column == "pick_name") return color(d.value);})
			.text(function (d) { return d.value; });

		cells.exit().remove();

	}	

	temp_filter = 'davidmargolis@gmail.com'

	updateTable(data,temp_filter)

});		



