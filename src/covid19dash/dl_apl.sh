x=$(curl -s https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json)
echo https://covid19-static.cdn-apple.com`jq -n "$x" |jq -r '.basePath'``jq -n "$x"|jq -r '.regions."en-us".csvPath'`
# curl -s $url -o ./latestAppleCovidData.csv