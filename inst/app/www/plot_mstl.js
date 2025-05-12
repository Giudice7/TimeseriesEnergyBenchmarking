function mstl_plot(decomposition, id_container) {
    // Create the chart
    var chart = Highcharts.chart(id_container, {
        chart: {
            type: 'line',
            zoomType: 'xy',
            backgroundColor: 'transparent',
            plotBorderColor: 'transparent',
            style: {
                fontFamily: 'Poppins, sans-serif'
            },
            height: 600  // Set the overall height of the chart
        },
        credits: {
          enabled: false
        },
        title: {
            text: ''
        },
        xAxis: {
            type: 'datetime'
        },
        tooltip: {
            xDateFormat: '%Y-%m-%d %H:%M',
            style: {
                fontSize: '1.3em'
            },
            valueDecimals: 2,
            valueSuffix: ' kW',
            headerFormat: '<p> <b>{point.key}</b></p> <br>',
            pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
                '<p style="color: {series.color}">{point.y}</p> <br>'
        },
        legend: {
            enabled: false
        },
        yAxis: [{
            title: {
                text: 'Data'
            },
            height: '18%',
            top: '0%',
            offset: 0,
            lineWidth: 1,
            gridLineWidth: 0  // Hide horizontal grid lines
        }, {
            title: {
                text: 'Trend'
            },
            height: '18%',
            top: '20%',
            offset: 0,
            lineWidth: 1,
            gridLineWidth: 0
        }, {
            title: {
                text: 'Seasonal24'
            },
            height: '18%',
            top: '40%',
            offset: 0,
            lineWidth: 1,
            gridLineWidth: 0
        }, {
            title: {
                text: 'Seasonal168'
            },
            height: '18%',
            top: '60%',
            offset: 0,
            lineWidth: 1,
            gridLineWidth: 0
        }, {
            title: {
                text: 'Remainder'
            },
            height: '18%',
            top: '80%',
            offset: 0,
            lineWidth: 1,
            gridLineWidth: 0
        }],
        series: [{
            type: 'line',
            name: 'Data',
            color: '#2E4F97',
            data: decomposition.map(function (entry) {
                return [entry.datetime_ms, entry.Data];
            }),
            yAxis: 0
        }, {
            type: 'line',
            name: 'Trend',
            color: '#2E4F97',
            data: decomposition.map(function (entry) {
                return [entry.datetime_ms, entry.Trend];
            }),
            yAxis: 1
        }, {
            type: 'line',
            name: 'Seasonal24',
            color: '#2E4F97',
            data: decomposition.map(function (entry) {
                return [entry.datetime_ms, entry.Seasonal24];
            }),
            yAxis: 2
        }, {
            type: 'line',
            name: 'Seasonal168',
            color: '#2E4F97',
            data: decomposition.map(function (entry) {
                return [entry.datetime_ms, entry.Seasonal168];
            }),
            yAxis: 3
        }, {
            type: 'line',
            name: 'Remainder',
            color: '#2E4F97',
            data: decomposition.map(function (entry) {
                return [entry.datetime_ms, entry.Remainder];
            }),
            yAxis: 4
        }]
    });
}
