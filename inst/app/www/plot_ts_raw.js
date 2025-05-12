function plot_ts_raw(data, id_container) {
    Highcharts.chart(id_container, {
        chart: {
            type: 'area',
            zoomType: 'xy',
            backgroundColor: 'transparent',
            plotBorderColor: 'transparent',
            style: {
                fontFamily: 'Poppins, sans-serif'
            }
        },
        credits: {
          enabled: false
        },
        legend: {
          enabled: false
        },
        title: {
            text: ''
        },
        xAxis: {
            type: 'datetime',
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
                text: 'Power [kW]',
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
            area: {
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
            headerFormat: '<p><b>{point.key}</b></p><br>',
            pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
                '<p style="color: {series.color}">{point.y}</p><br>'
        },
        series: [{
            type: 'area',
            name: 'Power',
            color: '#2E4F97',
            data: data.map(function(point) {
                return [point.datetime_ms, point.power];
            })
        }]
    });
}
