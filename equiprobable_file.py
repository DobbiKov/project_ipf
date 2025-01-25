res = [chr(i) for i in range(65, 91+26+6)]
res = ''.join(res)
res = res*100000
with open("test_files/equal.txt", "+w") as f:
    f.write(res)
    f.close()

