C
C
C
      SUBROUTINE ZZCHAR( CH , XP , YP , CT , ST )
C
C  Plot one character in CH with lower left corner at XP,YP physical
C  coordinates, with CT and ST the cosine and sine scaling/rotation
C  factors.
C
      CHARACTER*1 CH
C.......................................................................
C  The following digitization stuff is stolen from the NCAR metacode
C  interpreter MCVAX.  Various minor changes have been made.  Most
C  notable of these is the interchange of the '0' and 'O' characters --
C  I just couldn't stand the slash going through the 'O' as the CDC
C  custom has it.
C
      DIMENSION IA(128) , KU(494) , KV(494)
C
C  The following pointers relate standard FORTRAN characters to their
C  digitizations.  Note the plethora of 473's.  That location does
C  nothing.
C
C  <control characters>
      DATA IA(  1),IA(  2),IA(  3),IA(  4),IA(  5)/473,473,473,473,473/
C  <ctrls>
      DATA IA(  6),IA(  7),IA(  8),IA(  9),IA( 10)/473,473,473,473,473/
C  <ctrls>
      DATA IA( 11),IA( 12),IA( 13),IA( 14),IA( 15)/473,473,473,473,473/
C  <ctrls>
      DATA IA( 16),IA( 17),IA( 18),IA( 19),IA( 20)/473,473,473,473,473/
C  <ctrls>
      DATA IA( 21),IA( 22),IA( 23),IA( 24),IA( 25)/473,473,473,473,473/
C  <ctrls>
      DATA IA( 26),IA( 27),IA( 28),IA( 29),IA( 30)/473,473,473,473,473/
C  <ctrl><ctrl><space>!"
      DATA IA( 31),IA( 32),IA( 33),IA( 34),IA( 35)/473,473,473,473,473/
C  #$%&'
      DATA IA( 36),IA( 37),IA( 38),IA( 39),IA( 40)/473,473,473,473,473/
C  ()*+,
      DATA IA( 41),IA( 42),IA( 43),IA( 44),IA( 45)/448,456,429,414,476/
C  -./01
      DATA IA( 46),IA( 47),IA( 48),IA( 49),IA( 50)/423,486,444,143,286/
C  23456
      DATA IA( 51),IA( 52),IA( 53),IA( 54),IA( 55)/296,308,326,339,352/
C  789:;
      DATA IA( 56),IA( 57),IA( 58),IA( 59),IA( 60)/368,378,398,473,473/
C  <=>?@
      DATA IA( 61),IA( 62),IA( 63),IA( 64),IA( 65)/473,464,473,473,473/
C  ABCDE
      DATA IA( 66),IA( 67),IA( 68),IA( 69),IA( 70)/  1, 13, 28, 40, 49/
C  FGHIJ
      DATA IA( 71),IA( 72),IA( 73),IA( 74),IA( 75)/ 60, 68, 82, 92,104/
C  KLMNO
      DATA IA( 76),IA( 77),IA( 78),IA( 79),IA( 80)/113,123,130,137,273/
C  PQRST
      DATA IA( 81),IA( 82),IA( 83),IA( 84),IA( 85)/157,166,182,194,210/
C  UVWXY
      DATA IA( 86),IA( 87),IA( 88),IA( 89),IA( 90)/219,229,236,245,252/
C  Z[\]^
      DATA IA( 91),IA( 92),IA( 93),IA( 94),IA( 95)/262,448,473,456,473/
C  _`
      DATA IA( 96),IA( 97)                        /474,473            /
C  abcde
      DATA IA( 98),IA( 99),IA(100),IA(101),IA(102)/  1, 13, 28, 40, 49/
C  fghij
      DATA IA(103),IA(104),IA(105),IA(106),IA(107)/ 60, 68, 82, 92,104/
C  klmno
      DATA IA(108),IA(109),IA(110),IA(111),IA(112)/113,123,130,137,273/
C  pqrst
      DATA IA(113),IA(114),IA(115),IA(116),IA(117)/157,166,182,194,210/
C  uvwxy
      DATA IA(118),IA(119),IA(120),IA(121),IA(122)/219,229,236,245,252/
C  z
      DATA IA(123)                                /262                /
C  {|
      DATA IA(124),IA(125)                        /473,473            /
C  }~<DEL>
      DATA IA(126),IA(127),IA(128)                /473,473,473        /
C
C  The following DATA statements contain the digitizations of the
C  characters.  The characters are digitized on a box 6 units wide and
C  7 units tall.  This includes 2 units of white space to the right of
C  each character.  If KU=7, KV is a flag:
C     KV=0 ==> the next KU and KV are a pen up move
C              (normal coordinates are pen down moves)
C     KV=7 ==> the end of the digitization for a particular character
C              has been reached.
C
      DATA KU(  1),KU(  2),KU(  3),KU(  4),KU(  5),KU(  6)/0,4,7,0,0,1/
      DATA KV(  1),KV(  2),KV(  3),KV(  4),KV(  5),KV(  6)/3,3,0,3,6,7/
      DATA KU(  7),KU(  8),KU(  9),KU( 10),KU( 11),KU( 12)/3,4,4,7,6,7/
      DATA KV(  7),KV(  8),KV(  9),KV( 10),KV( 11),KV( 12)/7,6,0,0,0,7/
      DATA KU( 13),KU( 14),KU( 15),KU( 16),KU( 17),KU( 18)/0,3,4,4,3,0/
      DATA KV( 13),KV( 14),KV( 15),KV( 16),KV( 17),KV( 18)/7,7,6,5,4,4/
      DATA KU( 19),KU( 20),KU( 21),KU( 22),KU( 23),KU( 24)/7,3,4,4,3,0/
      DATA KV( 19),KV( 20),KV( 21),KV( 22),KV( 23),KV( 24)/0,4,3,1,0,0/
      DATA KU( 25),KU( 26),KU( 27),KU( 28),KU( 29),KU( 30)/7,6,7,7,4,3/
      DATA KV( 25),KV( 26),KV( 27),KV( 28),KV( 29),KV( 30)/0,0,7,0,6,7/
      DATA KU( 31),KU( 32),KU( 33),KU( 34),KU( 35),KU( 36)/1,0,0,1,3,4/
      DATA KV( 31),KV( 32),KV( 33),KV( 34),KV( 35),KV( 36)/7,6,1,0,0,1/
      DATA KU( 37),KU( 38),KU( 39),KU( 40),KU( 41),KU( 42)/7,6,7,0,3,4/
      DATA KV( 37),KV( 38),KV( 39),KV( 40),KV( 41),KV( 42)/0,0,7,7,7,6/
      DATA KU( 43),KU( 44),KU( 45),KU( 46),KU( 47),KU( 48)/4,3,0,7,6,7/
      DATA KV( 43),KV( 44),KV( 45),KV( 46),KV( 47),KV( 48)/1,0,0,0,0,7/
      DATA KU( 49),KU( 50),KU( 51),KU( 52),KU( 53),KU( 54)/0,4,7,3,0,7/
      DATA KV( 49),KV( 50),KV( 51),KV( 52),KV( 53),KV( 54)/7,7,0,4,4,0/
      DATA KU( 55),KU( 56),KU( 57),KU( 58),KU( 59),KU( 60)/0,4,7,6,7,0/
      DATA KV( 55),KV( 56),KV( 57),KV( 58),KV( 59),KV( 60)/0,0,0,0,7,7/
      DATA KU( 61),KU( 62),KU( 63),KU( 64),KU( 65),KU( 66)/4,7,0,3,7,6/
      DATA KV( 61),KV( 62),KV( 63),KV( 64),KV( 65),KV( 66)/7,0,4,4,0,0/
      DATA KU( 67),KU( 68),KU( 69),KU( 70),KU( 71),KU( 72)/7,7,4,3,1,0/
      DATA KV( 67),KV( 68),KV( 69),KV( 70),KV( 71),KV( 72)/7,0,6,7,7,6/
      DATA KU( 73),KU( 74),KU( 75),KU( 76),KU( 77),KU( 78)/0,1,3,4,4,3/
      DATA KV( 73),KV( 74),KV( 75),KV( 76),KV( 77),KV( 78)/1,0,0,1,3,3/
      DATA KU( 79),KU( 80),KU( 81),KU( 82),KU( 83),KU( 84)/7,6,7,0,7,0/
      DATA KV( 79),KV( 80),KV( 81),KV( 82),KV( 83),KV( 84)/0,0,7,7,0,4/
      DATA KU( 85),KU( 86),KU( 87),KU( 88),KU( 89),KU( 90)/4,7,4,4,7,6/
      DATA KV( 85),KV( 86),KV( 87),KV( 88),KV( 89),KV( 90)/4,0,7,0,0,0/
      DATA KU( 91),KU( 92),KU( 93),KU( 94),KU( 95),KU( 96)/7,7,1,3,7,2/
      DATA KV( 91),KV( 92),KV( 93),KV( 94),KV( 95),KV( 96)/7,0,7,7,0,7/
      DATA KU( 97),KU( 98),KU( 99),KU(100),KU(101),KU(102)/2,7,1,3,7,6/
      DATA KV( 97),KV( 98),KV( 99),KV(100),KV(101),KV(102)/0,0,0,0,0,0/
      DATA KU(103),KU(104),KU(105),KU(106),KU(107),KU(108)/7,7,0,1,3,4/
      DATA KV(103),KV(104),KV(105),KV(106),KV(107),KV(108)/7,0,1,0,0,1/
      DATA KU(109),KU(110),KU(111),KU(112),KU(113),KU(114)/4,7,6,7,0,7/
      DATA KV(109),KV(110),KV(111),KV(112),KV(113),KV(114)/7,0,0,7,7,0/
      DATA KU(115),KU(116),KU(117),KU(118),KU(119),KU(120)/0,4,7,2,4,7/
      DATA KV(115),KV(116),KV(117),KV(118),KV(119),KV(120)/3,7,0,5,0,0/
      DATA KU(121),KU(122),KU(123),KU(124),KU(125),KU(126)/6,7,7,0,0,4/
      DATA KV(121),KV(122),KV(123),KV(124),KV(125),KV(126)/0,7,0,7,0,0/
      DATA KU(127),KU(128),KU(129),KU(130),KU(131),KU(132)/7,6,7,0,2,4/
      DATA KV(127),KV(128),KV(129),KV(130),KV(131),KV(132)/0,0,7,7,3,7/
      DATA KU(133),KU(134),KU(135),KU(136),KU(137),KU(138)/4,7,6,7,0,4/
      DATA KV(133),KV(134),KV(135),KV(136),KV(137),KV(138)/0,0,0,7,7,0/
      DATA KU(139),KU(140),KU(141),KU(142),KU(143),KU(144)/4,7,6,7,4,7/
      DATA KV(139),KV(140),KV(141),KV(142),KV(143),KV(144)/7,0,0,7,7,0/
      DATA KU(145),KU(146),KU(147),KU(148),KU(149),KU(150)/4,4,3,1,0,0/
      DATA KV(145),KV(146),KV(147),KV(148),KV(149),KV(150)/1,6,7,7,6,1/
      DATA KU(151),KU(152),KU(153),KU(154),KU(155),KU(156)/1,3,4,7,6,7/
      DATA KV(151),KV(152),KV(153),KV(154),KV(155),KV(156)/0,0,1,0,0,7/
      DATA KU(157),KU(158),KU(159),KU(160),KU(161),KU(162)/0,3,4,4,3,0/
      DATA KV(157),KV(158),KV(159),KV(160),KV(161),KV(162)/7,7,6,5,4,4/
      DATA KU(163),KU(164),KU(165),KU(166),KU(167),KU(168)/7,6,7,7,0,0/
      DATA KV(163),KV(164),KV(165),KV(166),KV(167),KV(168)/0,0,7,0,1,6/
      DATA KU(169),KU(170),KU(171),KU(172),KU(173),KU(174)/1,3,4,4,3,1/
      DATA KV(169),KV(170),KV(171),KV(172),KV(173),KV(174)/7,7,6,1,0,0/
      DATA KU(175),KU(176),KU(177),KU(178),KU(179),KU(180)/0,7,2,4,7,6/
      DATA KV(175),KV(176),KV(177),KV(178),KV(179),KV(180)/1,0,2,0,0,0/
      DATA KU(181),KU(182),KU(183),KU(184),KU(185),KU(186)/7,0,3,4,4,3/
      DATA KV(181),KV(182),KV(183),KV(184),KV(185),KV(186)/7,7,7,6,5,4/
      DATA KU(187),KU(188),KU(189),KU(190),KU(191),KU(192)/0,7,2,4,7,6/
      DATA KV(187),KV(188),KV(189),KV(190),KV(191),KV(192)/4,0,4,0,0,0/
      DATA KU(193),KU(194),KU(195),KU(196),KU(197),KU(198)/7,7,0,1,3,4/
      DATA KV(193),KV(194),KV(195),KV(196),KV(197),KV(198)/7,0,1,0,0,1/
      DATA KU(199),KU(200),KU(201),KU(202),KU(203),KU(204)/4,3,1,0,0,1/
      DATA KV(199),KV(200),KV(201),KV(202),KV(203),KV(204)/3,4,4,5,6,7/
      DATA KU(205),KU(206),KU(207),KU(208),KU(209),KU(210)/3,4,7,6,7,7/
      DATA KV(205),KV(206),KV(207),KV(208),KV(209),KV(210)/7,6,0,0,7,0/
      DATA KU(211),KU(212),KU(213),KU(214),KU(215),KU(216)/0,4,7,2,2,7/
      DATA KV(211),KV(212),KV(213),KV(214),KV(215),KV(216)/7,7,0,7,0,0/
      DATA KU(217),KU(218),KU(219),KU(220),KU(221),KU(222)/6,7,7,0,0,1/
      DATA KV(217),KV(218),KV(219),KV(220),KV(221),KV(222)/0,7,0,7,1,0/
      DATA KU(223),KU(224),KU(225),KU(226),KU(227),KU(228)/3,4,4,7,6,7/
      DATA KV(223),KV(224),KV(225),KV(226),KV(227),KV(228)/0,1,7,0,0,7/
      DATA KU(229),KU(230),KU(231),KU(232),KU(233),KU(234)/7,0,2,4,7,6/
      DATA KV(229),KV(230),KV(231),KV(232),KV(233),KV(234)/0,7,0,7,0,0/
      DATA KU(235),KU(236),KU(237),KU(238),KU(239),KU(240)/7,7,0,0,2,4/
      DATA KV(235),KV(236),KV(237),KV(238),KV(239),KV(240)/7,0,7,0,4,0/
      DATA KU(241),KU(242),KU(243),KU(244),KU(245),KU(246)/4,7,6,7,4,7/
      DATA KV(241),KV(242),KV(243),KV(244),KV(245),KV(246)/7,0,0,7,7,0/
      DATA KU(247),KU(248),KU(249),KU(250),KU(251),KU(252)/0,4,7,6,7,7/
      DATA KV(247),KV(248),KV(249),KV(250),KV(251),KV(252)/7,0,0,0,7,0/
      DATA KU(253),KU(254),KU(255),KU(256),KU(257),KU(258)/0,2,4,7,2,2/
      DATA KV(253),KV(254),KV(255),KV(256),KV(257),KV(258)/7,4,7,0,4,0/
      DATA KU(259),KU(260),KU(261),KU(262),KU(263),KU(264)/7,6,7,7,3,1/
      DATA KV(259),KV(260),KV(261),KV(262),KV(263),KV(264)/0,0,7,0,4,4/
      DATA KU(265),KU(266),KU(267),KU(268),KU(269),KU(270)/7,0,4,0,4,7/
      DATA KV(265),KV(266),KV(267),KV(268),KV(269),KV(270)/0,7,7,0,0,0/
      DATA KU(271),KU(272),KU(273),KU(274),KU(275),KU(276)/6,7,7,4,3,1/
      DATA KV(271),KV(272),KV(273),KV(274),KV(275),KV(276)/0,7,0,1,0,0/
      DATA KU(277),KU(278),KU(279),KU(280),KU(281),KU(282)/0,0,1,3,4,4/
      DATA KV(277),KV(278),KV(279),KV(280),KV(281),KV(282)/1,6,7,7,6,1/
      DATA KU(283),KU(284),KU(285),KU(286),KU(287),KU(288)/7,6,7,7,1,2/
      DATA KV(283),KV(284),KV(285),KV(286),KV(287),KV(288)/0,0,7,0,6,7/
      DATA KU(289),KU(290),KU(291),KU(292),KU(293),KU(294)/2,7,1,3,7,6/
      DATA KV(289),KV(290),KV(291),KV(292),KV(293),KV(294)/0,0,0,0,0,0/
      DATA KU(295),KU(296),KU(297),KU(298),KU(299),KU(300)/7,7,0,1,3,4/
      DATA KV(295),KV(296),KV(297),KV(298),KV(299),KV(300)/7,0,6,7,7,6/
      DATA KU(301),KU(302),KU(303),KU(304),KU(305),KU(306)/4,0,0,4,7,6/
      DATA KV(301),KV(302),KV(303),KV(304),KV(305),KV(306)/5,1,0,0,0,0/
      DATA KU(307),KU(308),KU(309),KU(310),KU(311),KU(312)/7,7,0,1,3,4/
      DATA KV(307),KV(308),KV(309),KV(310),KV(311),KV(312)/7,0,7,7,7,6/
      DATA KU(313),KU(314),KU(315),KU(316),KU(317),KU(318)/4,3,1,7,3,4/
      DATA KV(313),KV(314),KV(315),KV(316),KV(317),KV(318)/5,4,4,0,4,3/
      DATA KU(319),KU(320),KU(321),KU(322),KU(323),KU(324)/4,3,1,0,7,6/
      DATA KV(319),KV(320),KV(321),KV(322),KV(323),KV(324)/1,0,0,1,0,0/
      DATA KU(325),KU(326),KU(327),KU(328),KU(329),KU(330)/7,7,3,3,2,0/
      DATA KV(325),KV(326),KV(327),KV(328),KV(329),KV(330)/7,0,0,7,7,4/
      DATA KU(331),KU(332),KU(333),KU(334),KU(335),KU(336)/0,4,7,2,4,7/
      DATA KV(331),KV(332),KV(333),KV(334),KV(335),KV(336)/3,3,0,0,0,0/
      DATA KU(337),KU(338),KU(339),KU(340),KU(341),KU(342)/6,7,7,0,1,3/
      DATA KV(337),KV(338),KV(339),KV(340),KV(341),KV(342)/0,7,0,1,0,0/
      DATA KU(343),KU(344),KU(345),KU(346),KU(347),KU(348)/4,4,3,0,0,4/
      DATA KV(343),KV(344),KV(345),KV(346),KV(347),KV(348)/1,3,4,4,7,7/
      DATA KU(349),KU(350),KU(351),KU(352),KU(353),KU(354)/7,6,7,7,4,3/
      DATA KV(349),KV(350),KV(351),KV(352),KV(353),KV(354)/0,0,7,0,7,7/
      DATA KU(355),KU(356),KU(357),KU(358),KU(359),KU(360)/1,0,0,1,3,4/
      DATA KV(355),KV(356),KV(357),KV(358),KV(359),KV(360)/7,6,1,0,0,1/
      DATA KU(361),KU(362),KU(363),KU(364),KU(365),KU(366)/4,3,1,0,7,6/
      DATA KV(361),KV(362),KV(363),KV(364),KV(365),KV(366)/3,4,4,3,0,0/
      DATA KU(367),KU(368),KU(369),KU(370),KU(371),KU(372)/7,7,0,0,4,4/
      DATA KV(367),KV(368),KV(369),KV(370),KV(371),KV(372)/7,0,6,7,7,6/
      DATA KU(373),KU(374),KU(375),KU(376),KU(377),KU(378)/2,2,7,6,7,7/
      DATA KV(373),KV(374),KV(375),KV(376),KV(377),KV(378)/1,0,0,0,7,0/
      DATA KU(379),KU(380),KU(381),KU(382),KU(383),KU(384)/2,0,0,1,3,4/
      DATA KV(379),KV(380),KV(381),KV(382),KV(383),KV(384)/4,5,6,7,7,6/
      DATA KU(385),KU(386),KU(387),KU(388),KU(389),KU(390)/4,2,2,0,0,1/
      DATA KV(385),KV(386),KV(387),KV(388),KV(389),KV(390)/5,4,4,2,1,0/
      DATA KU(391),KU(392),KU(393),KU(394),KU(395),KU(396)/3,4,4,2,7,6/
      DATA KV(391),KV(392),KV(393),KV(394),KV(395),KV(396)/0,1,2,4,0,0/
      DATA KU(397),KU(398),KU(399),KU(400),KU(401),KU(402)/7,7,0,1,3,4/
      DATA KV(397),KV(398),KV(399),KV(400),KV(401),KV(402)/7,0,1,0,0,1/
      DATA KU(403),KU(404),KU(405),KU(406),KU(407),KU(408)/4,3,1,0,0,1/
      DATA KV(403),KV(404),KV(405),KV(406),KV(407),KV(408)/6,7,7,6,4,3/
      DATA KU(409),KU(410),KU(411),KU(412),KU(413),KU(414)/3,4,7,6,7,7/
      DATA KV(409),KV(410),KV(411),KV(412),KV(413),KV(414)/3,4,0,0,7,0/
      DATA KU(415),KU(416),KU(417),KU(418),KU(419),KU(420)/0,4,7,2,2,7/
      DATA KV(415),KV(416),KV(417),KV(418),KV(419),KV(420)/3,3,0,5,1,0/
      DATA KU(421),KU(422),KU(423),KU(424),KU(425),KU(426)/6,7,7,0,4,7/
      DATA KV(421),KV(422),KV(423),KV(424),KV(425),KV(426)/0,7,0,3,3,0/
      DATA KU(427),KU(428),KU(429),KU(430),KU(431),KU(432)/6,7,7,0,4,7/
      DATA KV(427),KV(428),KV(429),KV(430),KV(431),KV(432)/0,7,0,1,5,0/
      DATA KU(433),KU(434),KU(435),KU(436),KU(437),KU(438)/2,2,7,4,0,7/
      DATA KV(433),KV(434),KV(435),KV(436),KV(437),KV(438)/5,1,0,3,3,0/
      DATA KU(439),KU(440),KU(441),KU(442),KU(443),KU(444)/0,4,7,6,7,4/
      DATA KV(439),KV(440),KV(441),KV(442),KV(443),KV(444)/5,1,0,0,7,7/
      DATA KU(445),KU(446),KU(447),KU(448),KU(449),KU(450)/7,6,7,7,3,2/
      DATA KV(445),KV(446),KV(447),KV(448),KV(449),KV(450)/0,0,7,1,7,6/
      DATA KU(451),KU(452),KU(453),KU(454),KU(455),KU(456)/2,3,7,6,7,7/
      DATA KV(451),KV(452),KV(453),KV(454),KV(455),KV(456)/1,0,0,0,7,0/
      DATA KU(457),KU(458),KU(459),KU(460),KU(461),KU(462)/1,2,2,1,7,6/
      DATA KV(457),KV(458),KV(459),KV(460),KV(461),KV(462)/7,6,1,0,0,0/
      DATA KU(463),KU(464),KU(465),KU(466),KU(467),KU(468)/7,7,4,0,7,0/
      DATA KV(463),KV(464),KV(465),KV(466),KV(467),KV(468)/7,0,5,5,0,2/
      DATA KU(469),KU(470),KU(471),KU(472),KU(473),KU(474)/4,7,6,7,7,6/
      DATA KV(469),KV(470),KV(471),KV(472),KV(473),KV(474)/2,0,0,7,0,0/
      DATA KU(475),KU(476),KU(477),KU(478),KU(479),KU(480)/7,7,1,2,2,1/
      DATA KV(475),KV(476),KV(477),KV(478),KV(479),KV(480)/7,0,0,1,2,2/
      DATA KU(481),KU(482),KU(483),KU(484),KU(485),KU(486)/1,2,7,6,7,7/
      DATA KV(481),KV(482),KV(483),KV(484),KV(485),KV(486)/1,1,0,0,7,0/
      DATA KU(487),KU(488),KU(489),KU(490),KU(491),KU(492)/2,1,1,2,2,7/
      DATA KV(487),KV(488),KV(489),KV(490),KV(491),KV(492)/0,0,1,1,0,0/
      DATA KU(493),KU(494)                                /6,7        /
      DATA KV(493),KV(494)                                /0,7        /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Select digitization for this character.
C
      XOLD   = XP
      YOLD   = YP
      IPOINT = IA( ICHAR(CH) + 1 )
C
C  Scale lower case letters to be slightly smaller
C
      IF( ICHAR(CH).GE.ICHAR('a') .AND. ICHAR(CH).LE.ICHAR('z') )THEN
         CTL = 0.8 * CT
         STL = 0.8 * ST
      ELSE
         CTL = CT
         STL = ST
      ENDIF
C
100   CONTINUE
         NU     = KU(IPOINT)
         NV     = KV(IPOINT)
         IPOINT = IPOINT + 1
C.......................................................................
C  Test for op-code stored in NV.  This is flagged by a 7 in NU.
C
         IF( NU .EQ. 7 )THEN
C
C  Op-codes are: NV = 7             ==> end of character
C                     anything else ==> pen up move to next location
C
            IF( NV .EQ. 7 )THEN
               RETURN
            ELSE
               XOLD   = XP + CTL * KU(IPOINT) - STL * KV(IPOINT)
               YOLD   = YP + STL * KU(IPOINT) + CTL * KV(IPOINT)
               IPOINT = IPOINT + 1
            ENDIF
C.......................................................................
C  Here, plot the next stroke.
C
         ELSE
            XNEW = XP + CTL * NU - STL * NV
            YNEW = YP + STL * NU + CTL * NV
            CALL ZZLINE( XOLD,YOLD , XNEW,YNEW )
            XOLD = XNEW
            YOLD = YNEW
         ENDIF
C.......................................................................
C  Loopback to get next plotting order from KU, KV.
C
      GOTO 100
      END
