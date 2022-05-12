# !/bin/bash 

# sh ~/Drive/Codigos/AlgoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.sh >> ~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/Logs/$(date '+%Y_%m_%d')_Trades.log
# sh ~/algoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.sh >> ~/algoTrading/DataOut/MASlope_ATRStopLoss/Logs/$(date '+%Y_%m_%d')_Trades.log

echo 'Start Strategy'
echo 'Date:   ' $(date '+%Y-%m-%d %H:%M:%S')
echo ''

symbol='FXSBUSD'
echo 'Symbol: ' $symbol
# Rscript ~/Drive/Codigos/AlgoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.R $symbol >> ~/Drive/Codigos/AlgoTrading/DataOut/MASlope_ATRStopLoss/allLogs/$(date '+%Y_%m_%d_%H:%M:%S')_$symbol.log
Rscript ~/algoTrading/Scripts/Strategies/MASlope_ATRStopL_Prod.R $symbol >> ~/algoTrading/DataOut/MASlope_ATRStopLoss/allLogs/$(date '+%Y_%m_%d_%H:%M:%S')_$symbol.log

echo 'End Strategy'
echo ''
echo ''