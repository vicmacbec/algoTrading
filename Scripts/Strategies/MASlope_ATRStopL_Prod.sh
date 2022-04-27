# !/bin/bash 

echo 'Date:   ' $(date '+%Y-%m-%d %H:%M:%S')

symbol='FXSBUSD'
echo 'Symbol: ' $symbol
Rscript /home/victor/Drive/Codigos/AlgoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.R $symbol >> Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/allLogs/$(date '+%Y_%m_%d')_$symbol