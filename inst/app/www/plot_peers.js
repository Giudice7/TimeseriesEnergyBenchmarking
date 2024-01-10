function scatter_plot_peers(id_container, end_use_features, peers_features, building_features, end_use_name) {

  Highcharts.chart(id_container, {
    chart: {
      type: 'scatter',
      zoomType: 'xy',
      backgroundColor: 'transparent',
      plotBorderColor: 'transparent',
      style: {
        fontFamily: 'Poppins, sans-serif'
      },
    },
    title: {
      text: ''
    },
    xAxis: {
      title: {
        text: 'Shape factor [-]',
        align: 'middle'
      }
    },
    yAxis: {
      title: {
        text: 'Energy consumption [kWh]',
        align: 'middle'
      },
      offset: 10,
      opposite: false
    },
    tooltip: {
      style: {
        fontSize: '1.3em'
      },
      valueDecimals: 2,
      valueSuffix: ' kWh',
      headerFormat: '<p style="color: {series.color}"> <b>Shape factor: {point.key}</b></p> <br>',
      pointFormat: '<p style="color: {series.color}"><b>Mean daily energy consumption: {point.y}</b></p> <br>' +
                    '<p><b>{point.building_id}</b> </p>'
    },
    series: [{
      type: 'scatter',
      name: end_use_name,
      color: '#2E4F97',
      data: end_use_features.map(function(entry) {
        return {
          x: entry.shape_factor,
          y: entry.mean_ec,
          building_id: entry.building_id
        };
      })
    }, {
      type: 'scatter',
      name: 'Peers',
      color: '#F27507',
      data: peers_features.map(function(entry) {
        return {
          x: entry.shape_factor,
          y: entry.mean_ec,
          building_id: entry.building_id
        };
      })
    }, {
      type: 'scatter',
      name: 'Your building',
      color: '#C7003F',
      data: building_features.map(function(entry) {
        return {
          x: entry.shape_factor,
          y: entry.mean_ec,
          building_id: entry.building_id
        };
      })
    }]
  });
}
