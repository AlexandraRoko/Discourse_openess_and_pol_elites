'''
Calculatuing Kullback-Leibner divergence between two topic distributions
'''


def KLdivergence_from_probdist_arrays(pdists1, pdists0):
    """
    Calculate KL divergence between probability distributions held on the same
    rows of two arrays.

    NOTE: elements of pdist* are assumed to be positive (non-zero), a
    necessary condition for using Kullback-Leibler Divergence.

    Args:
      pdists* (numpy.ndarray): arrays, where rows for each constitute the two
      probability distributions from which to calculate divergence.  pdists1
      contains the distributions holding probabilities in the numerator of the
      KL divergence summand.

    Returns:
      numpy.ndarray: KL divergences, where the second array's rows are the
        distributions in the numerator of the log in KL divergence

    """

    assert pdists0.shape == pdists1.shape, 'pdist* shapes must be identical'

    if len(pdists0.shape) == 1:
        KLdiv = (pdists1 * np.log2(pdists1/pdists0)).sum()
    elif len(pdists0.shape) == 2:
        KLdiv = (pdists1 * np.log2(pdists1/pdists0)).sum(axis=1)

    return KLdiv
