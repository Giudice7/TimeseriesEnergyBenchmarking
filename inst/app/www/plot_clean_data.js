function plot_ts_clean(data, corruptanceIndex, container_id) {
    Highcharts.chart(container_id, {
        chart: {
            type: 'area',
            zoomType: 'xy',
            backgroundColor: 'transparent',
            plotBorderColor: 'transparent',
            style: {
                fontFamily: 'Poppins, sans-serif'
            }
        },
        xAxis: {
            type: 'datetime'
        },
        yAxis: {
            offset: 10,
            opposite: false,
            title: {
                text: 'Power [kW]',
                align: 'middle'
            }
        },
        plotOptions: {
            area: {
                fillOpacity: 0.4
            },
            scatter: {
                marker: {
                    symbol: 'circle'
                }
            }
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
        series: [{
            type: 'area',
            name: 'Raw',
            color: '#2E4F97',
            data: data.map(function(point, i) {
                return [point.datetime_ms, point.power_raw];
            })
        }, {
            type: 'line',
            name: 'Clean',
            color: '#F27507',
            data: data.map(function(point, i) {
                return [point.datetime_ms, point.power];
            })
        }, {
            type: 'scatter',
            name: 'Corruptance',
            color: '#C7003F',
            data: corruptanceIndex.map(function(index) {
                const point = data[index - 1];
                return {
                    x: point.datetime_ms,
                    y: point.power_raw,
                    type: point.type
                };
            }),
            tooltip: {
                headerFormat: '<p> <b>{point.key}</b></p> <br>',
                pointFormat: '<p style="color: {series.color}"><b>Power </b></p>' +
                    '<p style="color: {series.color}">{point.y}</p> <br>' +
                    '<p><b>Corruptance Type</b>: {point.type}</p> <br>'
            }
        }],
        chart: {
            zoomType: 'xy',
            backgroundColor: 'transparent',
            plotBorderColor: 'transparent',
            style: {
                fontFamily: 'Poppins, sans-serif'
            }
        }
    });
}
