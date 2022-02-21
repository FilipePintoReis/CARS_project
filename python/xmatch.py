# Since this file requires pandas, pandas will have to be installed alongside python to be used by reticulate
import pandas as pd


def xmatch(dA, dB, dDR, inFileName, outFileName) -> list:
    dHLA = set(['A' + str(dA[0]), 'A' + str(dA[1]), 'B' + str(dB[0]), 'B' + str(dB[1]), 'DR' + str(dDR[0]), 'DR' + str(dDR[1])])

    df = pd.read_csv(inFileName)

    df["res"] = df.apply(func = lambda x: x["abs"] in dHLA, axis = 1)

    df.groupby("id")

    df.to_csv(outFileName, index = False)


xmatch([1,2], [1,2], [1,2], "python/test.csv", "None")