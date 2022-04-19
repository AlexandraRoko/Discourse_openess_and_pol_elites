

data_12_19_keys = sorted(data_12_19_keys)
id2rede_concat_keys = sorted(list(id2rede_concat.keys()))
id2rede_concat_keys_sorted = sorted([int(key) for key in id2rede_concat_keys])[1:]
id2rede_concat_keys_sorted

data_ready_concat_POSTag = []
for key in id2rede_concat_keys_sorted:
    #if key in data_12_19_keys:
    data_ready_concat_POSTag.append(id2rede_concat[str(key)]["data_ready"])

data_ready_concat_POSTag = [ls[0] for ls in data_ready_concat_POSTag]