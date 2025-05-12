function load_condition_plot(data, id_container, load_condition, max) {

  // Group data by date
  var groupedData = {};
  var load_condition_string = load_condition;
  var max_value = parseFloat(max);


  console.log("max", max_value)

  data.forEach(function (entry) {
    var date = entry.date;

    if (!groupedData[date]) {
      groupedData[date] = [];
    }

    groupedData[date].push({
      time: entry.time,
      power: entry.power,
    });
  });

  // Create an array of series for each date
  var allSeriesData = Object.keys(groupedData).map(function (date) {
    var color = load_condition_string === "Holidays" || load_condition_string === "Non-working days" ? "grey" : '#2E4F97';

    return {
      type: 'line',
      name: date,
      color: color,
      data: groupedData[date].map(function (entry) {
        return [entry.time, entry.power];
      }),
      marker: {
        enabled: false
      }
    };
  });

  Highcharts.chart(id_container, {
    chart: {
      type: 'line',
      zoomType: 'xy',
      backgroundColor: 'transparent',
      plotBorderColor: 'transparent',
      style: {
        fontFamily: 'Poppins, sans-serif'
      },
    },
    title: {
      text: load_condition_string
    },
    credits: {
      enabled: false
    },
    xAxis: {
      categories: data.map(function (entry) {
        return entry.time;
      }),
      labels: {
        rotation: -45,
        style: {
          fontSize: '1.3em'
        }
      },
      title: {
        text: 'Time',
        style: {
          fontSize: '1.3em'
        }
      }
    },
    tooltip: {
      style: {
        fontSize: '1.3em'
      },
      valueDecimals: 2,
      valueSuffix: ' kW',
      headerFormat: '<p> <b>{point.key}</b></p> <br>',
      pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
        '<p style="color: {series.color}">{point.y}</p> <br>' +
        '<b>Date</b>: {series.name}'
    },
    legend: {
      enabled: false
    },
    yAxis: {
      title: {
        text: 'Power [kW]',
        style: {
        fontSize: '1.3em'
      },
      labels: {
        style: {
        fontSize: '1.3em'
      }
      }
      },
      max: max_value, // Set the y-axis max limit
      height: '100%',
      top: '0%',
      offset: 0,
      lineWidth: 1,
      gridLineWidth: 0  // Hide horizontal grid lines
    },
    series: allSeriesData
  });
}
