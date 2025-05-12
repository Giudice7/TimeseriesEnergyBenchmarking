function plot_volatility_profiles(data, id_container, load_condition, max) {
  var groupedData = {};
  var max_value = parseFloat(max);

  // Group data by date
  data.forEach(function (entry) {
    var date = entry.date;
    if (!groupedData[date]) {
      groupedData[date] = [];
    }

    groupedData[date].push({
      time: entry.time,
      power: entry.power,
      type: entry.type
    });
  });

  // Group data by type
  var groupedByType = {};
  Object.keys(groupedData).forEach(function (date) {
    var type = groupedData[date][0].type;
    if (!groupedByType[type]) {
      groupedByType[type] = {};
    }
    groupedByType[type][date] = groupedData[date];
  });

  var allSeriesData = [];

  // Define colors and line widths
  var typeSettings = {
    "Selected": { color: "#FF0000", lineWidth: 3 },
    "Neighbor": { color: "#007BFF", lineWidth: 1 },
    "Normal":   { color: "lightgrey", lineWidth: 1 }
  };

  // Loop over each type and build dummy + real series
  Object.keys(groupedByType).forEach(function (type) {
    var settings = typeSettings[type] || { color: "lightgrey", lineWidth: 1 };
    var typeId = "group-" + type;

    // Dummy legend series (must have non-empty data to be shown)
    allSeriesData.push({
      id: typeId,
      name: type,
      type: 'line',
      color: settings.color,
      data: [[null, null]], // Invisible dummy point
      showInLegend: true,
      enableMouseTracking: false,
      marker: { enabled: false }
    });

    // Add one real series per date
    Object.keys(groupedByType[type]).forEach(function (date) {
      allSeriesData.push({
        name: date,
        type: 'line',
        color: settings.color,
        lineWidth: settings.lineWidth,
        data: groupedByType[type][date].map(entry => [entry.time, entry.power]),
        showInLegend: false,
        linkedTo: typeId,
        marker: { enabled: false }
      });
    });
  });

  Highcharts.chart(id_container, {
    chart: {
      type: 'line',
      zoomType: 'xy',
      backgroundColor: 'transparent',
      style: {
        fontFamily: 'Poppins, sans-serif'
      }
    },
    title: {
      text: load_condition
    },
    xAxis: {
      categories: [...new Set(data.map(entry => entry.time))],
      title: {
        text: 'Hour of the day',
        style: { fontSize: '1.3em' }
      },
      labels: {
        rotation: -45,
        style: { fontSize: '1.3em' }
      }
    },
    yAxis: {
      title: {
        text: 'Power [kW]',
        style: { fontSize: '1.3em' }
      },
      labels: {
        style: { fontSize: '1.3em' }
      },
      max: max_value,
      gridLineWidth: 0
    },
    legend: {
      enabled: true,
      itemStyle: { fontSize: '1.3em' }
    },
    credits: {
      enabled: false
    },
    series: allSeriesData
  });
}
