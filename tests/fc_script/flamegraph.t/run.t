  $ NOGUI=1 frama-c-script flamegraph flamegraph.txt

Check if the HTML file was generated
  $ cat .frama-c/flamegraph.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8">
      <title>Eva Flamegraph</title>
    </head>
    <body>
      <embed src="flamegraph.svg" style="max-width:100%;width:1400px;min-width:1000px">
    </body>
  </html>

The generated SVG is too large, so we arbitrarily grep a few lines
  $ grep f1 .frama-c/flamegraph.svg
  <title>f1 (6 samples, 44.44%)</title><rect x="112.2" y="65" width="664.5" height="15.0" fill="rgb(232,181,30)" rx="2" ry="2" />
  <text text-anchor="" x="115.22" y="75.5" font-size="11" font-family="Verdana" fill="rgb(0,0,0)"  >f1</text>
