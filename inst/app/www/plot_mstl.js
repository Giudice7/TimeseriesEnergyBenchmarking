function mstl_plot(decomposition, id_container) {
    var container = document.getElementById(id_container);
    container.innerHTML = '';  // Clear existing content

    for (var i = 1; i < decomposition.ncol - 1; i++) {
        var plot_item = decomposition.colnames[i];
        var chartContainerId = id_container + '_' + i;

        // Create a new div for each chart
        var chartDiv = document.createElement('div');
        chartDiv.id = chartContainerId;
        container.appendChild(chartDiv);

        // Create the chart
        var chart = Highcharts.chart(chartContainerId, {
            chart: {
                type: 'line',
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
                line: {
                    marker: {
                        enabled: false
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
            legend: {
                enabled: false
            },
            title: {
                text: plot_item
            },
            series: [{
                type: 'line',
                name: plot_item,
                color: '#2E4F97',
                data: decomposition[plot_item].map(function(value, index) {
                    return [decomposition.datetime_ms[index], value];
                })
            }],
            accessibility: {
                enabled: false  // Set to false to disable the accessibility module
            }
        });

        // Add additional options for the 'Remainder' plot_item
        if (plot_item === 'Remainder') {
            var upper_bound = quantile(decomposition.Remainder, 0.75) + 5 * IQR(decomposition.Remainder);
            var lower_bound = quantile(decomposition.Remainder, 0.25) - 5 * IQR(decomposition.Remainder);

            chart.yAxis[0].addPlotLine({
                value: upper_bound,
                color: '#C7003F',
                width: 3,
                zIndex: 4,
                label: {
                    text: 'Upper bound',
                    style: {
                        color: '#C7003F',
                        fontWeight: 'bold'
                    }
                }
            });

            chart.yAxis[0].addPlotLine({
                value: lower_bound,
                color: '#C7003F',
                width: 3,
                zIndex: 4,
                label: {
                    text: 'Lower bound',
                    style: {
                        color: '#C7003F',
                        fontWeight: 'bold'
                    }
                }
            });
        }
    }
}
