# Since this file requires pandas, pandas will have to be installed alongside python to be used by reticulate

from pandas import read_csv


def xmatch(dA, dB, dDR, inFileName, outFileName) -> list:
    dHLA = set(['A' + str(dA[0]), 'A' + str(dA[1]), 'B' + str(dB[0]), 'B' + str(dB[1]), 'DR' + str(dDR[0]), 'DR' + str(dDR[1])])

    df = read_csv(inFileName)

    df["xm"] = df.apply(func = lambda x: x["abs"] in dHLA, axis = 1)

    df = df.groupby("ID")

    def list_or(l):
        for el in l: 
            if el: return True
        return False

    df = df.agg(list_or)

    df.to_csv(outFileName)
