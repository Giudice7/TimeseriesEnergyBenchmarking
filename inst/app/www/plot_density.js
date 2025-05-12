function plot_density(data_peers, data_end_use, container_id, reference_value, label_end_use, label_axis) {
  function cleanData(data) {
    return data.filter(x => typeof x === 'number' && Number.isFinite(x));
  }

  data_peers = cleanData(data_peers);
  data_end_use = cleanData(data_end_use);

  // Helper: Gaussian kernel
  function gaussian(x) {
    return Math.exp(-0.5 * x * x) / Math.sqrt(2 * Math.PI);
  }

  // KDE estimate function
  function kde(data, samplePoints) {
    const n = data.length;
    const mean = data.reduce((a, b) => a + b) / n;
    const std = Math.sqrt(data.reduce((a, b) => a + (b - mean) ** 2, 0) / n) || 1e-6;
    const bandwidth = 1.06 * std * Math.pow(n, -1 / 5);
    return samplePoints.map(x => {
      const sum = data.reduce((acc, xi) => acc + gaussian((x - xi) / bandwidth), 0);
      return [x, sum / (n * bandwidth)];
    });
  }

  // Percentile helper
  function percentile(arr, p) {
    if (arr.length === 0) return null;
    const sorted = [...arr].sort((a, b) => a - b);
    const idx = (p / 100) * (sorted.length - 1);
    const lower = Math.floor(idx);
    const upper = Math.ceil(idx);
    if (lower === upper) return sorted[lower];
    return sorted[lower] + (sorted[upper] - sorted[lower]) * (idx - lower);
  }

  const p10 = percentile(data_peers, 10);
  const p90 = percentile(data_peers, 90);
  const p50 = percentile(data_peers, 50);

  const combined = data_peers.concat(data_end_use);
  const min = Math.min(...combined);
  const max = Math.max(...combined);
  const steps = 100;
  const stepSize = (max - min) / steps;
  const samplePoints = Array.from({ length: steps }, (_, i) => min + i * stepSize);

  const kde_peers = kde(data_peers, samplePoints);
  const kde_end_use = kde(data_end_use, samplePoints);
  const maxY = Math.max(...kde_peers.map(d => d[1]), ...kde_end_use.map(d => d[1]));

  Highcharts.chart(container_id, {
    chart: {
      type: 'area',
      backgroundColor: 'transparent',
      zoomType: 'xy',
      style: {
        fontFamily: 'Poppins, sans-serif'
      }
    },
    title: { text: `` },
    xAxis: {
      title: { text: label_axis, style: { fontSize: '1.2em' } },
      labels: { style: { fontSize: '1em' } }
    },
    yAxis: {
      title: { text: 'Density', style: { fontSize: '1.2em' } },
      labels: { style: { fontSize: '1em' } },
      gridLineWidth: 0
    },
    legend: {
      enabled: true,
      itemStyle: { fontSize: '1.3em' }
    },
    credits: { enabled: false },
    tooltip: {
      shared: true,
      headerFormat: '<b>{point.key:.2f}</b><br>',
      pointFormat: '{series.name}: <b>{point.y:.4f}</b><br>',
      style: { fontSize: '1.3em' }
    },
    series: [
      {
        type: 'areaspline',
        name: 'Peers',
        data: kde_peers,
        color: '#F27507',
        fillOpacity: 0.3,
        marker: { enabled: false }
      },
      {
        type: 'areaspline',
        name: label_end_use,
        data: kde_end_use,
        color: '#2E4F97',
        fillOpacity: 0.3,
        marker: { enabled: false }
      },
      {
        name: "Your building",
        type: 'line',
        color: '#C7003F',
        data: [
          [reference_value, 0],
          [reference_value, maxY]
        ],
        dashStyle: 'Dash',
        marker: { enabled: false },
        enableMouseTracking: false,
        showInLegend: true
      },
      {
      name: "10th percentile",
      type: 'line',
      color: '#F27507',
      data: [
        [p10, 0],
        [p10, maxY]
      ],
      dashStyle: 'ShortDot',
      marker: { enabled: false },
      enableMouseTracking: false,
      showInLegend: true
    },
    {
      name: "90th percentile",
      type: 'line',
      color: '#F27507',
      data: [
        [p90, 0],
        [p90, maxY]
      ],
      dashStyle: 'ShortDot',
      marker: { enabled: false },
      enableMouseTracking: false,
      showInLegend: true
    },
    {
      name: "Median",
      type: 'line',
      color: '#F27507',
      data: [
        [p50, 0],
        [p50, maxY]
      ],
      dashStyle: 'Dash',
      lineWidth: 3,
      marker: { enabled: false },
      enableMouseTracking: false,
      showInLegend: true
    }
    ]
  });
}
