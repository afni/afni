from _tools import *
from mdp.nodes import TimeDelayNode, TimeDelaySlidingWindowNode

def test_TimeDelayNodes():
    x = numx.array(
        [ [1, 2, 3] , 
          [2, 4, 4],  
          [3, 8, 5], 
          [4, 16, 6],
          [5, 32, 7] 
          ])

    ################################################
    node = TimeDelayNode(time_frames=3, gap=2)
    slider = TimeDelaySlidingWindowNode(time_frames=3, gap=2)

    real_res = [ [1, 2, 3, 0, 0, 0, 0, 0, 0],
                 [2, 4, 4, 0, 0, 0, 0, 0, 0],
                 [3, 8, 5, 1, 2, 3, 0, 0, 0],
                 [4, 16,6, 2, 4, 4, 0, 0, 0],
                 [5, 32,7, 3, 8, 5, 1, 2, 3]
                ]
    real_res = numx.array(real_res)

    res = node.execute(x)

    assert_array_equal(real_res, res)

    # test sliding window
    slider_res = numx.zeros_like(real_res)
    for row_nr in range(x.shape[0]):
        slider_res[row_nr, :] = slider.execute(x[[row_nr], :])

    assert_array_equal(real_res, slider_res)

    ################################################
    node = TimeDelayNode(time_frames=2, gap=3)
    slider = TimeDelaySlidingWindowNode(time_frames=2, gap=3)

    real_res = [ [1, 2, 3, 0, 0, 0],
                 [2, 4, 4, 0, 0, 0],
                 [3, 8, 5, 0, 0, 0],
                 [4, 16,6, 1, 2, 3],
                 [5, 32,7, 2, 4, 4]
                ]
    real_res = numx.array(real_res)

    res = node.execute(x)
    assert_array_equal(real_res, res)

    # test sliding window
    slider_res = numx.zeros_like(real_res)
    for row_nr in range(x.shape[0]):
        slider_res[row_nr, :] = slider.execute(x[[row_nr], :])

    assert_array_equal(real_res, slider_res)

    ################################################
    node = TimeDelayNode(time_frames=4, gap=1)
    slider = TimeDelaySlidingWindowNode(time_frames=4, gap=1)

    real_res = [ [1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                 [2, 4, 4, 1, 2, 3, 0, 0, 0, 0, 0, 0],
                 [3, 8, 5, 2, 4, 4, 1, 2, 3, 0, 0, 0],
                 [4, 16,6, 3, 8, 5, 2, 4, 4, 1, 2, 3],
                 [5, 32,7, 4, 16,6, 3, 8, 5, 2, 4, 4]
                ]
    real_res = numx.array(real_res)

    res = node.execute(x)
    assert_array_equal(real_res, res)

    # test sliding window
    slider_res = numx.zeros_like(real_res)
    for row_nr in range(x.shape[0]):
        slider_res[row_nr, :] = slider.execute(x[[row_nr], :])

    assert_array_equal(real_res, slider_res)

