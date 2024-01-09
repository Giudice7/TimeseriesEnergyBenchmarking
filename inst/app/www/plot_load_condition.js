function load_condition_plot(data, id_container) {

  // Group data by load condition and date
  var groupedData = {};
  data.forEach(function(entry) {
    var loadCondition = entry.load_condition;
    var date = entry.date;

    if (!groupedData[loadCondition]) {
      groupedData[loadCondition] = {};
    }

    if (!groupedData[loadCondition][date]) {
      groupedData[loadCondition][date] = [];
    }

    groupedData[loadCondition][date].push(entry);
  });

  // Create an array of series for each load condition
  var summerSeriesData = Object.keys(groupedData["Summer workdays"]).map(function(date) {
    return {
      type: 'line',
      name: 'Summer workdays - ' + date,
      color: '#2E4F97',
      title: {
        text: 'Summer workdays'
      },
      data: groupedData["Summer workdays"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 0,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  var winterSeriesData = Object.keys(groupedData["Winter workdays"]).map(function(date) {
    return {
      type: 'line',
      name: 'Winter workdays - ' + date,
      color: '#2E4F97',
      title: {
        text: 'Winter workdays'
      },
      data: groupedData["Winter workdays"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 1,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  var summerWeekendSeriesData = Object.keys(groupedData["Summer weekends"]).map(function(date) {
    return {
      type: 'line',
      name: 'Summer weekends - ' + date,
      color: '#2E4F97',
      title: {
        text: 'Summer weekends'
      },
      data: groupedData["Summer weekends"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 2,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  var winterWeekendSeriesData = Object.keys(groupedData["Winter weekends"]).map(function(date) {
    return {
      type: 'line',
      name: 'Winter weekends - ' + date,
      color: '#2E4F97',
      title: {
        text: 'Winter weekends'
      },
      data: groupedData["Winter weekends"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 3,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  var nonWorkingDaysSeriesData = Object.keys(groupedData["Non-working days"]).map(function(date) {
    return {
      type: 'line',
      name: 'Non-working days - ' + date,
      color: '#AFAFAF',
      title: {
        text: 'Non-working days'
      },
      data: groupedData["Non-working days"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 4,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  var holidaysSeriesData = Object.keys(groupedData["Holidays"]).map(function(date) {
    return {
      type: 'line',
      name: 'Holidays - ' + date,
      color: '#AFAFAF',
      title: {
        text: 'Holidays'
      },
      data: groupedData["Holidays"][date].map(function(entry) {
        return [entry.time, entry.power, entry.date];
      }),
      xAxis: 5,
      marker: {
        enabled: false  // Remove marker
      }
    };
  });

  // Concatenate all series data
  var allSeriesData = summerSeriesData
    .concat(winterSeriesData)
    .concat(summerWeekendSeriesData)
    .concat(winterWeekendSeriesData)
    .concat(nonWorkingDaysSeriesData)
    .concat(holidaysSeriesData);

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
      text: ''
    },
    xAxis: [{
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Summer workdays';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '0%',
      width: '15%',
      offset: 0,
    }, {
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Winter workdays';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '15%',
      width: '15%',
      offset: 0,
    }, {
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Summer weekends';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '30%',
      width: '15%',
      offset: 0,
    }, {
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Winter weekends';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '45%',
      width: '15%',
      offset: 0,
    }, {
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Non-working days';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '60%',
      width: '15%',
      offset: 0,
    }, {
      categories: data.filter(function(entry) {
        return entry.load_condition === 'Holidays';
      }).map(function(entry) {
        return Highcharts.dateFormat('%H:%M', entry.time);
      }),
      left: '75%',
      width: '15%',
      offset: 0,
    }],
    tooltip: {
      style: {
        fontSize: '1.3em'
      },
      valueDecimals: 2,
      valueSuffix: ' kW',
      headerFormat: '<p> <b>{point.key}</b></p> <br>',
      pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
                    '<p style="color: {series.color}">{point.y}</p> <br>' +
                    '<b>Date</b>: {point.date}'
    },
    legend: {
      enabled: false
    },
    yAxis: {
      title: {
        text: 'Power [kW]'
      },
      height: '100%',
      top: '0%',
      offset: 0,
      lineWidth: 1,
      gridLineWidth: 0  // Hide horizontal grid lines
    },
    series: allSeriesData
  });
}
