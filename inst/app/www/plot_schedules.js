function plot_heatmap(data, container_id) {
    // Extract unique hours as strings (e.g., "00:00", "01:00")
    let uniqueHours = [...new Set(data.map(point => point.hour))].sort();

    // Extract unique dates as strings
    let uniqueDates = [...new Set(data.map(point => point.date))].sort();

    // Select dates at intervals of 7 days for labeling
    let filteredDates = uniqueDates.filter((date, index) => index % 7 === 0);

    // Convert categorical "on_hour" to numeric & map hour to category index
    let heatmapData = data.map(point => ({
      x: uniqueDates.indexOf(point.date),
      y: uniqueHours.indexOf(point.hour),
      value: point.on_hour === "on hour" ? 1 : point.on_hour === "weekend" ? 2 : 0,
      dateLabel: point.date,
      hourLabel: point.hour,
      statusLabel: point.on_hour,
      powerLabel: point.power
    }));

    Highcharts.chart(container_id, {
        chart: {
            type: 'heatmap',
            zoomType: 'xy',
            backgroundColor: 'transparent',
            plotBorderColor: 'transparent',
            style: { fontFamily: 'Poppins, sans-serif' }
        },
        credits: { enabled: false },
        title: { text: "" },
        xAxis: {
            categories: uniqueDates,  // Use string-based dates
            title: {
                text: "Date",
                style: { fontSize: '1.3em' }  // Set font size for x-axis title
            },
            labels: {
                formatter: function () {
                    return filteredDates.includes(this.value) ? this.value : ''; // Show only selected dates
                },
                rotation: -45,
                style: { fontSize: '1em' }  // Set font size for x-axis labels
            }
        },
        yAxis: {
            title: {
                text: "Hour of Day",
                style: { fontSize: '1.3em' }  // Set font size for y-axis title
            },
            categories: uniqueHours,  // Maintain string format for hours
            reversed: true,  // Optional: Flip to have 00:00 at the top
            labels: {
                style: { fontSize: '1em' }  // Set font size for y-axis labels
            }
        },
        // Remove colorAxis from the chart entirely
        colorAxis: {
            min: 0,
            max: 2,
            stops: [
                [0 / 2, '#C7003F'],  // 0 → Red
                [1 / 2, '#2E4F97'],  // 1 → Blue
                [2 / 2, '#2ECC71']   // 2 → Green (for weekend)
            ],
            visible: false
        },
        legend: { visible: false },
        tooltip: {
            headerFormat: '<b>{point.point.dateLabel} {point.point.hourLabel}</b><br>',
            pointFormat: '<b>Status:</b> <span style="color: {point.color}; font-weight: bold">{point.point.statusLabel}</span><br> <b>Power:</b> {point.point.powerLabel} kW',
            style: { fontSize: '1.1em' }  // Set font size for tooltips
        },
        series: [{
            type: 'heatmap',
            borderWidth: 1,
            data: heatmapData,
            dataLabels: { enabled: false }
        }]
    });
}
