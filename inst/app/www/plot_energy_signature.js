function energy_signature_plot(data, id_container, slope, intercept) {
  // Plot the scatter chart
  Highcharts.chart(id_container, {
    chart: {
      type: 'scatter',
      zoomType: 'xy',
      backgroundColor: 'transparent',
      plotBorderColor: 'transparent',
      style: {
        fontFamily: 'Poppins, sans-serif'
      }
    },
    title: {
      text: 'Energy signature'
    },
        credits: {
          enabled: false
        },
    legend: {
      enabled: false
    },
    xAxis: {
      title: {
        text: 'Temperature [°C]',
        align: 'middle',
        style: {
          fontSize: '1.3em'
                }
      },
      labels: {
        style: {
          fontSize: '1.3em'
                }
      }
    },
    yAxis: {
      offset: 50,
      opposite: false,
      title: {
        text: 'Energy consumption [kWh]',
        align: 'middle',
        style: {
          fontSize: '1.3em'
                }
      },
      labels: {
        style: {
          fontSize: '1.3em'
                }
      }
    },
    plotOptions: {
      scatter: {
        fillOpacity: 0.4
      }
    },
    tooltip: {
      xDateFormat: '%Y-%m-%d %H:%M',
      style: {
        fontSize: '1.3em'
      },
      valueDecimals: 2,
      valueSuffix: ' kW',
      headerFormat: '<p> <b>{point.key} °C</b></p> <br>',
      pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
                    '<p style="color: {series.color}">{point.y}</p> <br>'
    },
    legend: {
      enabled: false
    },
    series: [{
      type: 'scatter',
      data: data.map(function(entry) {
        return {
          x: entry.temperature,
          y: entry.energy
        };
      }),
      color: '#2E4F97'
    }, {
      type: 'line',
      name: 'Regression Line',
      data: [
        [Math.min.apply(null, data.map(function(entry) { return entry.temperature; })),
         slope * Math.min.apply(null, data.map(function(entry) { return entry.temperature; })) + intercept],
        [Math.max.apply(null, data.map(function(entry) { return entry.temperature; })),
         slope * Math.max.apply(null, data.map(function(entry) { return entry.temperature; })) + intercept]
      ],
      color: 'red'
    }]
  });
}
