function radar_plot(data, id_container, title) {
  const categories = data.map(entry => entry.KPI);
  const numCategories = categories.length;
  const tickInterval = 360 / numCategories;

  Highcharts.chart(id_container, {
    chart: {
      polar: true,
      backgroundColor: 'transparent',
      plotBorderColor: 'transparent',
      style: {
        fontFamily: 'Poppins, sans-serif'
      }
    },
    title: {
      text: title,
      style: {
        fontSize: '1.5em'
      }
    },
    credits: {
      enabled: false
    },
    legend: {
      enabled: false  // Hides the legend
    },
    pane: {
      startAngle: 0,
      endAngle: 360
    },
    xAxis: {
      categories: categories,
      tickmarkPlacement: 'on',
      lineWidth: 0,
      labels: {
        style: {
          fontSize: '1.5em'
        }
      }
    },
    yAxis: {
      lineWidth: 0,
      min: 0,
      max: 100,
      tickInterval: 25,
      labels: {
        style: {
          fontSize: '1.3em'
        }
      }
    },
    tooltip: {
      shared: true,
      pointFormat: '<span style="color:{series.color}">{point.y:.1f}</span><br/>'
    },
    plotOptions: {
      series: {
        marker: {
          enabled: true
        },
        dataLabels: {
          enabled: true,
          style: {
            fontSize: '1.3em'
          }
        }
      }
    },
    series: [{
      name: 'KPIs',
      data: data.map(entry => entry.Value),
      pointPlacement: 'on',
      color: '#2E4F97'
    }]
  });
}
