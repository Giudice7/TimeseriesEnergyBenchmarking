function plot_anomalies(data, id_container, load_condition, max) {
  // Group data by date
  var groupedData = {};
  var max_value = parseFloat(max);

  console.log("max", max_value);

  data.forEach(function (entry) {
    var date = entry.date;

    if (!groupedData[date]) {
      groupedData[date] = [];
    }

    groupedData[date].push({
      time: entry.time,
      power: entry.power,
      type: entry.type // Include type for color selection
    });
  });

  // Create an array of series for each date
  var allSeriesData = Object.keys(groupedData).map(function (date) {
    // Determine color based on 'type'
    var isAnomalous = groupedData[date].some(entry => entry.type === "Anomalous");
    var color = isAnomalous ? "red" : "grey";

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
      text: load_condition
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
        text: 'Hour of the day',
        style: {
        fontSize: '1.3em'
      }
      }
    },
    legend: {
      enabled: false
    },
    yAxis: {
      title: {
        text: 'Power [kW]',
        style: {
          fontSize: '1.3em'
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
