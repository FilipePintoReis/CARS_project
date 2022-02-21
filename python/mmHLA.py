def mmHLA(dA, dB, dDR, cA, cB, cDR) -> list:
    mmA = 0
    mmB = 0
    mmDR = 0
    
    dA = set(dA)
    dB = set(dB)
    dDR = set(dDR)
    cA = set(cA)
    cB = set(cB)
    cDR = set(cDR)
    
    # Allele A
    for anti_gen in dA:
        if anti_gen not in cA:
            mmA += 1
    
    # Allele B
    for anti_gen in dB:
        if anti_gen not in cB:
            mmB += 1
    
    # Allele DR
    for anti_gen in dDR:
        if anti_gen not in cDR:
            mmDR += 1

    return [mmA, mmB, mmDR, mmA + mmB + mmDR]

# def __main__():
#     if len(argv) != 13:
#         print(f"Wrong number of arguments.\nExpected 12.\nGiven:{len(argv)}.")

#         return None
#     else:
#         dA = set([argv[1], argv[2]])
#         dB = set([argv[3], argv[4]])
#         dDR = set([argv[5], argv[6]])
#         cA = set([argv[7], argv[8]])
#         cB = set([argv[9], argv[10]])
#         cDR = set([argv[11], argv[12]])

#         return mmHLA(dA, dB, dDR, cA, cB, cDR)

# return_value = __main__()

# print(return_value)
    
