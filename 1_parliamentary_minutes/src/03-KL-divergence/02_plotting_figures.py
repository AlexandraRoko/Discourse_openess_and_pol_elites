from sklearn.linear_model import LinearRegression

with open('../../data/written/id2speech_concat_POSTag_inshape_withThetas.txt', 'r') as infile:
    id2rede_filtered = json.load(infile)
infile.close()

with open('../../data/written/speech2KL_lda_model_concat_POSTag_self_tuned_all_sorted.txt', 'r') as infile:
    speech2KL = json.load(infile)
infile.close()


## plot raw data

fig, ax1 = plt.subplots(1, 1)
plt.rcParams["figure.figsize"] = (5, 4)
fig.set_size_inches(8, 4, forward=True)

# select windowsize
window = 25

years = []
keys_sorted = sorted([int(key) for key in speech2KL[str(window)].keys()])

for ax, num in zip([ax1], [window]):
    novelty = []
    role = []
    date = []

    keys_sorted = sorted([int(key) for key in speech2KL[str(num)].keys()])

    for key in keys_sorted:
        novelty.append(speech2KL[str(num)][str(key)]["novelty"])
        role.append(id2rede_filtered[str(key)]["positionShort"])
        date.append(datetime.strptime(id2rede_filtered[str(key)]["date"], "%Y-%m-%d"))

    xvalues = date
    color_map = {'Presidium of Parliament': "green", 'Member of Parliament': "blue", 'Minister': "red",
                 'Guest': "yellow", 'Not found': "grey", 'Chancellor': "orange", 'Secretary of State': "black"}

    ax.scatter(xvalues, pd.Series(novelty), c=pd.DataFrame(role)[0].map(color_map), marker="o", alpha=0.3,
               s=5)
    ax.set_title(f'Novelty [windowsize {str(num)}]')
    ax.set_ylabel('Novelty $\mathcal{N}}$')
    ax.set_xlabel('Date')

    markers = [plt.Line2D([0, 0], [0, 0], color=color, marker='o', linestyle='', alpha=0.3) for color in
               color_map.values()]
    plt.legend(markers, color_map.keys(), numpoints=1)

    fig.tight_layout()

plt.savefig("./figures/novelty_by_roles.png", dpi=300, facecolor='w',
           edgecolor='w', orientation='portrait', format=None, transparent=False, pad_inches=0.1)


## plot novelty by roles
fig, ax1 = plt.subplots(1, 1)
plt.rcParams["figure.figsize"] = (10, 8)
fig.set_size_inches(16.5, 8.5, forward=True)

# select windowsize
window = 10

years = []
keys_sorted = sorted([int(key) for key in id2rede_filtered_roles.keys()])
tick_list = [0, 20000, 40000, 60000, 80000, 100000, 120000]


for ax, num in zip([ax1], [window]):
    novelty = []
    role = []
    date = []

    keys_sorted = sorted([int(key) for key in id2rede_filtered_roles.keys()])
    keys_speech2KL = sorted([int(key) for key in speech2KL[10].keys()])

    for key in keys_sorted:
        try:
            novelty.append(speech2KL[num][str(key)]["novelty"])
            role.append(id2rede_filtered[str(key)]["positionShort"])
            date.append(datetime.strptime(id2rede_filtered[str(key)]["date"], "%Y-%m-%d"))
        except KeyError:
            pass

    xvalues = date
    color_map = {'Member of Parliament': "blue", 'Minister': "red",
                 'Guest': "yellow", 'Not found': "grey", 'Chancellor': "orange", 'Secretary of State': "green"}

    ax.scatter(xvalues, pd.Series(novelty), c=pd.DataFrame(role)[0].map(color_map), marker="o", alpha=0.3,
               s=8)
    ax.set_title(f'Novelty [windowsize {str(num)}] | without "Presidium of the Parliament"')
    ax.set_ylabel('Novelty $\mathcal{N}}$')
    ax.set_xlabel('Date')

    markers = [plt.Line2D([0, 0], [0, 0], color=color, marker='o', linestyle='', alpha=0.3) for color in
               color_map.values()]
    plt.legend(markers, color_map.keys(), numpoints=1)

    fig.tight_layout()

plt.savefig("./figures/novelty_by_roles_w_10_without_presidium.png", dpi=300, facecolor='w',
            edgecolor='w', orientation='portrait', format=None,
            transparent=False, pad_inches=0.1)


## plot robustness check
plt.rcParams["figure.figsize"] = (20, 15)
fig, ((ax1, ax2, ax3), (ax4, ax5, ax6), (ax7, ax8, ax9)) = plt.subplots(3, 3)
fig.suptitle('Transience vs. Novelty: Robustness-check for different windowsize specifications')


for ax, num in zip([ax1, ax2, ax3, ax4, ax5, ax6, ax7, ax8, ax9], [3, 5, 7, 10, 15, 25, 50, 100,
                                                                   1000]):

    transience = []
    novelty = []
    keys_sorted = sorted([int(key) for key in id2rede_filtered_roles.keys()])

    for key in keys_sorted:
        try:
            novelty.append(speech2KL[str(num)][str(key)]["novelty"])
            transience.append(speech2KL[str(num)][str(key)]["transience"])
        except KeyError:
            pass

    ax.plot(novelty, transience, marker="o", linewidth=0, alpha=0.3)
    ax.plot([0, 1], [0, 1], linestyle='--', color='k', transform=ax.transAxes)

    ax.set_title(f'Windowsize {str(num)}')
    ax.set_ylabel('Transience $\mathcal{T}}$')
    ax.set_xlabel('Novelty $\mathcal{N}$')
    fig.tight_layout()

    ax.set_title(f'Windowsize {str(num)}')

plt.savefig("./figures/TransVsNovelty_robustness_different_windows.png", dpi=300, facecolor='w',
            edgecolor='w', orientation='portrait', format=None,
            transparent=False, pad_inches=0.1)

## Generate stacked bar plot, following the example code given at https://github.com/CogentMentat/NTRexample_FRevNCA
# Save everything in one variable: it's "keys_sorted, novelty, transience, resonance"
window = "25"
resonance = []
novelty = []
transience = []

keys_sorted = sorted([int(key) for key in id2rede_filtered_roles.keys()])

for key in keys_sorted:
    if str(key) in speech2KL[window].keys():
        resonance.append(speech2KL[window][str(key)]["resonance"])
        novelty.append(speech2KL[window][str(key)]["novelty"])
        transience.append(speech2KL[window][str(key)]["transience"])

stacked_data = np.vstack(zip(keys_sorted, novelty, transience, resonance))

def plot_quants_2Dhist(q0, q1, ax, xbins, ybins, make_cbar=True,
                       cbar_axis=False, cbar_orientation='vertical', colorvmax=None):
    q0bins = xbins
    q1bins = ybins

    H, xedges, yedges = np.histogram2d(q0, q1, bins=[q0bins, q1bins])

    # H needs to be rotated and flipped
    H = np.rot90(H)
    H = np.flipud(H)

    # Mask zeros
    Hmasked = np.ma.masked_where(H == 0, H)  # Mask pixels with a value

    # Plot 2D histogram using pcolor
    if colorvmax:
        usemax = colorvmax
    else:
        usemax = H.max()
    pcolm = ax.pcolormesh(xedges, yedges, Hmasked, norm=mpl.colors.LogNorm(vmin=1, vmax=usemax))

    if make_cbar:
        if cbar_axis:
            cbar = fig.colorbar(pcolm, cax=cbar_axis, orientation=cbar_orientation)
        else:
            cbar = fig.colorbar(pcolm, ax=ax, orientation=cbar_orientation)
        cbar.ax.set_ylabel('counts')

    if make_cbar:
        return H, cbar
    else:
        return H


centininch = 2.54
inchincent = .3937

def centtoinch(cents):
    return .3937 * cents

def inchtocent(inches):
    return 2.54 * inches

figsize = (centtoinch(11.4), 2.5)
plt.set_cmap('RdYlBu_r')
fig = plt.figure(figsize=figsize)

# Uncomment to see figure extent.

## Plot Transience v. Novelty

q0 = stacked_data[:,1] #novelty
q1 = stacked_data[:,2] #transience

ax = fig.add_axes([0.1, 0.15, 0.4, 0.72])

cbaxes = fig.add_axes([0.5, 0.29, 0.02, 0.2])


xbins = np.linspace(0, 10, 50) # determines size and granularity of the plot
ybins = np.linspace(0, 10, 50)

H, cbar = plot_quants_2Dhist(q0, q1, ax, xbins, ybins,
                             make_cbar=True, cbar_axis=cbaxes, cbar_orientation='vertical')
cbar.ax.set_ylabel('')
cbar.ax.set_xlabel('counts', fontsize=7)
cbar.ax.xaxis.set_label_position('bottom')
cbar.ax.yaxis.set_ticks_position('left')
cbar.ax.tick_params(labelsize=7)

### Identity (x=y) line
ax.plot([0,1],[0,1], linestyle='--', color='k', transform=ax.transAxes)

ax.legend([mpl.lines.Line2D([0], [0], color='k', linewidth=1.5, linestyle='--')],
          ['x=y'],
          loc='upper center', fontsize=8, ncol=2, handlelength=2.7)

ax.set_ylabel('Transience $\mathcal{T}$         ')
ax.set_xlabel('Novelty $\mathcal{N}$')

ax.set_title('Transience vs. Novelty \n')

ax.set_aspect('equal')

### Hide the right and top spines.
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
### Show ticks only on the left and bottom spines.
ax.yaxis.set_ticks_position('left')
ax.xaxis.set_ticks_position('bottom')


## Plot Reson v. Novelty

ax = fig.add_axes([0.8, 0.15, 0.35, 0.72])

cbaxes = fig.add_axes([1.15, 0.27, 0.02, 0.2])

#quants = ['Novelty', 'Resonance']

q0 = stacked_data[:,1] #novelty
q2 = stacked_data[:,3] #transience

xbins = np.linspace(0, 10, 50)
ybins = np.linspace(-6, 6, 50)

H, cbar = plot_quants_2Dhist(q0, q2, ax, xbins, ybins,
                             make_cbar=True, cbar_axis=cbaxes, cbar_orientation='vertical')
cbar.ax.set_ylabel('')
cbar.ax.set_xlabel('counts', fontsize=7)
cbar.ax.xaxis.set_label_position('bottom')
cbar.ax.yaxis.set_ticks_position('left')
cbar.ax.tick_params(labelsize=7)

ax.axhline(color='k', linewidth=1.5, linestyle=':')


### Hide the right and top spines.
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)

ax.set_ylabel('Resonance $\mathcal{R}}$')
ax.set_xlabel('Novelty $\mathcal{N}$')

_ = ax.set_title('Resonance vs. Novelty \n')

df_res_vs_novel = df.dropna(subset=['novelty'])
df_res_vs_novel = df_res_vs_novel.dropna(subset=["resonance"])
novelty_df = np.array(df_res_vs_novel["novelty"]).reshape(-1, 1)
resonance_df = np.array(df_res_vs_novel["resonance"]).reshape(-1, 1)
linear_regressor = LinearRegression()  # create object for the class
linear_regressor.fit(novelty_df, resonance_df)  # perform linear regression
Y_pred = linear_regressor.predict(novelty_df)  # make predictions

ax.plot(list(novelty_df), list(Y_pred), linestyle='-', color='k')

ax.legend([mpl.lines.Line2D(list(novelty_df), list(Y_pred), color='k', linewidth=1.5, linestyle='-')],
          ['fit'],
          loc='upper center', fontsize=8, ncol=2, handlelength=2.7)

#fig.tight_layout()

plt.savefig("./figures/TransVsNov_ResVsNovel_combi_neu.png", dpi=300, facecolor='w',
            edgecolor='w', format=None, bbox_inches='tight',
            transparent=False, pad_inches=0.1)