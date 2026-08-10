# Simple snapshot test for creating input data

    Code
      as.data.frame(create_input_data(population = dplyr::filter(fso_pop,
        spatial_unit %in% c("Aarau")), births = dplyr::filter(fso_birth,
        spatial_unit %in% c("Aarau")), year_first = 2011, year_last = 2014,
      age_fert_min = 15, age_fert_max = 49, fert_hist_years = 3, binational = TRUE))
    Output
          tfr.spatial_unit tfr.nat tfr.year tfr.tfr mab.spatial_unit mab.nat mab.year
      1              Aarau      ch     2011   1.332            Aarau      ch     2011
      2              Aarau      ch     2012   1.291            Aarau      ch     2012
      3              Aarau      ch     2013   1.311            Aarau      ch     2013
      4              Aarau      ch     2014   1.112            Aarau      ch     2014
      5              Aarau     int     2011   1.937            Aarau     int     2011
      6              Aarau     int     2012   1.756            Aarau     int     2012
      7              Aarau     int     2013   1.533            Aarau     int     2013
      8              Aarau     int     2014   2.601            Aarau     int     2014
      9              Aarau      ch     2011   1.332            Aarau      ch     2011
      10             Aarau      ch     2012   1.291            Aarau      ch     2012
      11             Aarau      ch     2013   1.311            Aarau      ch     2013
      12             Aarau      ch     2014   1.112            Aarau      ch     2014
      13             Aarau     int     2011   1.937            Aarau     int     2011
      14             Aarau     int     2012   1.756            Aarau     int     2012
      15             Aarau     int     2013   1.533            Aarau     int     2013
      16             Aarau     int     2014   2.601            Aarau     int     2014
      17             Aarau      ch     2011   1.332            Aarau      ch     2011
      18             Aarau      ch     2012   1.291            Aarau      ch     2012
      19             Aarau      ch     2013   1.311            Aarau      ch     2013
      20             Aarau      ch     2014   1.112            Aarau      ch     2014
      21             Aarau     int     2011   1.937            Aarau     int     2011
      22             Aarau     int     2012   1.756            Aarau     int     2012
      23             Aarau     int     2013   1.533            Aarau     int     2013
      24             Aarau     int     2014   2.601            Aarau     int     2014
      25             Aarau      ch     2011   1.332            Aarau      ch     2011
      26             Aarau      ch     2012   1.291            Aarau      ch     2012
      27             Aarau      ch     2013   1.311            Aarau      ch     2013
      28             Aarau      ch     2014   1.112            Aarau      ch     2014
      29             Aarau     int     2011   1.937            Aarau     int     2011
      30             Aarau     int     2012   1.756            Aarau     int     2012
      31             Aarau     int     2013   1.533            Aarau     int     2013
      32             Aarau     int     2014   2.601            Aarau     int     2014
      33             Aarau      ch     2011   1.332            Aarau      ch     2011
      34             Aarau      ch     2012   1.291            Aarau      ch     2012
      35             Aarau      ch     2013   1.311            Aarau      ch     2013
      36             Aarau      ch     2014   1.112            Aarau      ch     2014
      37             Aarau     int     2011   1.937            Aarau     int     2011
      38             Aarau     int     2012   1.756            Aarau     int     2012
      39             Aarau     int     2013   1.533            Aarau     int     2013
      40             Aarau     int     2014   2.601            Aarau     int     2014
      41             Aarau      ch     2011   1.332            Aarau      ch     2011
      42             Aarau      ch     2012   1.291            Aarau      ch     2012
      43             Aarau      ch     2013   1.311            Aarau      ch     2013
      44             Aarau      ch     2014   1.112            Aarau      ch     2014
      45             Aarau     int     2011   1.937            Aarau     int     2011
      46             Aarau     int     2012   1.756            Aarau     int     2012
      47             Aarau     int     2013   1.533            Aarau     int     2013
      48             Aarau     int     2014   2.601            Aarau     int     2014
      49             Aarau      ch     2011   1.332            Aarau      ch     2011
      50             Aarau      ch     2012   1.291            Aarau      ch     2012
      51             Aarau      ch     2013   1.311            Aarau      ch     2013
      52             Aarau      ch     2014   1.112            Aarau      ch     2014
      53             Aarau     int     2011   1.937            Aarau     int     2011
      54             Aarau     int     2012   1.756            Aarau     int     2012
      55             Aarau     int     2013   1.533            Aarau     int     2013
      56             Aarau     int     2014   2.601            Aarau     int     2014
      57             Aarau      ch     2011   1.332            Aarau      ch     2011
      58             Aarau      ch     2012   1.291            Aarau      ch     2012
      59             Aarau      ch     2013   1.311            Aarau      ch     2013
      60             Aarau      ch     2014   1.112            Aarau      ch     2014
      61             Aarau     int     2011   1.937            Aarau     int     2011
      62             Aarau     int     2012   1.756            Aarau     int     2012
      63             Aarau     int     2013   1.533            Aarau     int     2013
      64             Aarau     int     2014   2.601            Aarau     int     2014
      65             Aarau      ch     2011   1.332            Aarau      ch     2011
      66             Aarau      ch     2012   1.291            Aarau      ch     2012
      67             Aarau      ch     2013   1.311            Aarau      ch     2013
      68             Aarau      ch     2014   1.112            Aarau      ch     2014
      69             Aarau     int     2011   1.937            Aarau     int     2011
      70             Aarau     int     2012   1.756            Aarau     int     2012
      71             Aarau     int     2013   1.533            Aarau     int     2013
      72             Aarau     int     2014   2.601            Aarau     int     2014
      73             Aarau      ch     2011   1.332            Aarau      ch     2011
      74             Aarau      ch     2012   1.291            Aarau      ch     2012
      75             Aarau      ch     2013   1.311            Aarau      ch     2013
      76             Aarau      ch     2014   1.112            Aarau      ch     2014
      77             Aarau     int     2011   1.937            Aarau     int     2011
      78             Aarau     int     2012   1.756            Aarau     int     2012
      79             Aarau     int     2013   1.533            Aarau     int     2013
      80             Aarau     int     2014   2.601            Aarau     int     2014
      81             Aarau      ch     2011   1.332            Aarau      ch     2011
      82             Aarau      ch     2012   1.291            Aarau      ch     2012
      83             Aarau      ch     2013   1.311            Aarau      ch     2013
      84             Aarau      ch     2014   1.112            Aarau      ch     2014
      85             Aarau     int     2011   1.937            Aarau     int     2011
      86             Aarau     int     2012   1.756            Aarau     int     2012
      87             Aarau     int     2013   1.533            Aarau     int     2013
      88             Aarau     int     2014   2.601            Aarau     int     2014
      89             Aarau      ch     2011   1.332            Aarau      ch     2011
      90             Aarau      ch     2012   1.291            Aarau      ch     2012
      91             Aarau      ch     2013   1.311            Aarau      ch     2013
      92             Aarau      ch     2014   1.112            Aarau      ch     2014
      93             Aarau     int     2011   1.937            Aarau     int     2011
      94             Aarau     int     2012   1.756            Aarau     int     2012
      95             Aarau     int     2013   1.533            Aarau     int     2013
      96             Aarau     int     2014   2.601            Aarau     int     2014
      97             Aarau      ch     2011   1.332            Aarau      ch     2011
      98             Aarau      ch     2012   1.291            Aarau      ch     2012
      99             Aarau      ch     2013   1.311            Aarau      ch     2013
      100            Aarau      ch     2014   1.112            Aarau      ch     2014
      101            Aarau     int     2011   1.937            Aarau     int     2011
      102            Aarau     int     2012   1.756            Aarau     int     2012
      103            Aarau     int     2013   1.533            Aarau     int     2013
      104            Aarau     int     2014   2.601            Aarau     int     2014
      105            Aarau      ch     2011   1.332            Aarau      ch     2011
      106            Aarau      ch     2012   1.291            Aarau      ch     2012
      107            Aarau      ch     2013   1.311            Aarau      ch     2013
      108            Aarau      ch     2014   1.112            Aarau      ch     2014
      109            Aarau     int     2011   1.937            Aarau     int     2011
      110            Aarau     int     2012   1.756            Aarau     int     2012
      111            Aarau     int     2013   1.533            Aarau     int     2013
      112            Aarau     int     2014   2.601            Aarau     int     2014
      113            Aarau      ch     2011   1.332            Aarau      ch     2011
      114            Aarau      ch     2012   1.291            Aarau      ch     2012
      115            Aarau      ch     2013   1.311            Aarau      ch     2013
      116            Aarau      ch     2014   1.112            Aarau      ch     2014
      117            Aarau     int     2011   1.937            Aarau     int     2011
      118            Aarau     int     2012   1.756            Aarau     int     2012
      119            Aarau     int     2013   1.533            Aarau     int     2013
      120            Aarau     int     2014   2.601            Aarau     int     2014
      121            Aarau      ch     2011   1.332            Aarau      ch     2011
      122            Aarau      ch     2012   1.291            Aarau      ch     2012
      123            Aarau      ch     2013   1.311            Aarau      ch     2013
      124            Aarau      ch     2014   1.112            Aarau      ch     2014
      125            Aarau     int     2011   1.937            Aarau     int     2011
      126            Aarau     int     2012   1.756            Aarau     int     2012
      127            Aarau     int     2013   1.533            Aarau     int     2013
      128            Aarau     int     2014   2.601            Aarau     int     2014
      129            Aarau      ch     2011   1.332            Aarau      ch     2011
      130            Aarau      ch     2012   1.291            Aarau      ch     2012
      131            Aarau      ch     2013   1.311            Aarau      ch     2013
      132            Aarau      ch     2014   1.112            Aarau      ch     2014
      133            Aarau     int     2011   1.937            Aarau     int     2011
      134            Aarau     int     2012   1.756            Aarau     int     2012
      135            Aarau     int     2013   1.533            Aarau     int     2013
      136            Aarau     int     2014   2.601            Aarau     int     2014
      137            Aarau      ch     2011   1.332            Aarau      ch     2011
      138            Aarau      ch     2012   1.291            Aarau      ch     2012
      139            Aarau      ch     2013   1.311            Aarau      ch     2013
      140            Aarau      ch     2014   1.112            Aarau      ch     2014
      141            Aarau     int     2011   1.937            Aarau     int     2011
      142            Aarau     int     2012   1.756            Aarau     int     2012
      143            Aarau     int     2013   1.533            Aarau     int     2013
      144            Aarau     int     2014   2.601            Aarau     int     2014
      145            Aarau      ch     2011   1.332            Aarau      ch     2011
      146            Aarau      ch     2012   1.291            Aarau      ch     2012
      147            Aarau      ch     2013   1.311            Aarau      ch     2013
      148            Aarau      ch     2014   1.112            Aarau      ch     2014
      149            Aarau     int     2011   1.937            Aarau     int     2011
      150            Aarau     int     2012   1.756            Aarau     int     2012
      151            Aarau     int     2013   1.533            Aarau     int     2013
      152            Aarau     int     2014   2.601            Aarau     int     2014
      153            Aarau      ch     2011   1.332            Aarau      ch     2011
      154            Aarau      ch     2012   1.291            Aarau      ch     2012
      155            Aarau      ch     2013   1.311            Aarau      ch     2013
      156            Aarau      ch     2014   1.112            Aarau      ch     2014
      157            Aarau     int     2011   1.937            Aarau     int     2011
      158            Aarau     int     2012   1.756            Aarau     int     2012
      159            Aarau     int     2013   1.533            Aarau     int     2013
      160            Aarau     int     2014   2.601            Aarau     int     2014
      161            Aarau      ch     2011   1.332            Aarau      ch     2011
      162            Aarau      ch     2012   1.291            Aarau      ch     2012
      163            Aarau      ch     2013   1.311            Aarau      ch     2013
      164            Aarau      ch     2014   1.112            Aarau      ch     2014
      165            Aarau     int     2011   1.937            Aarau     int     2011
      166            Aarau     int     2012   1.756            Aarau     int     2012
      167            Aarau     int     2013   1.533            Aarau     int     2013
      168            Aarau     int     2014   2.601            Aarau     int     2014
      169            Aarau      ch     2011   1.332            Aarau      ch     2011
      170            Aarau      ch     2012   1.291            Aarau      ch     2012
      171            Aarau      ch     2013   1.311            Aarau      ch     2013
      172            Aarau      ch     2014   1.112            Aarau      ch     2014
      173            Aarau     int     2011   1.937            Aarau     int     2011
      174            Aarau     int     2012   1.756            Aarau     int     2012
      175            Aarau     int     2013   1.533            Aarau     int     2013
      176            Aarau     int     2014   2.601            Aarau     int     2014
      177            Aarau      ch     2011   1.332            Aarau      ch     2011
      178            Aarau      ch     2012   1.291            Aarau      ch     2012
      179            Aarau      ch     2013   1.311            Aarau      ch     2013
      180            Aarau      ch     2014   1.112            Aarau      ch     2014
      181            Aarau     int     2011   1.937            Aarau     int     2011
      182            Aarau     int     2012   1.756            Aarau     int     2012
      183            Aarau     int     2013   1.533            Aarau     int     2013
      184            Aarau     int     2014   2.601            Aarau     int     2014
      185            Aarau      ch     2011   1.332            Aarau      ch     2011
      186            Aarau      ch     2012   1.291            Aarau      ch     2012
      187            Aarau      ch     2013   1.311            Aarau      ch     2013
      188            Aarau      ch     2014   1.112            Aarau      ch     2014
      189            Aarau     int     2011   1.937            Aarau     int     2011
      190            Aarau     int     2012   1.756            Aarau     int     2012
      191            Aarau     int     2013   1.533            Aarau     int     2013
      192            Aarau     int     2014   2.601            Aarau     int     2014
      193            Aarau      ch     2011   1.332            Aarau      ch     2011
      194            Aarau      ch     2012   1.291            Aarau      ch     2012
      195            Aarau      ch     2013   1.311            Aarau      ch     2013
      196            Aarau      ch     2014   1.112            Aarau      ch     2014
      197            Aarau     int     2011   1.937            Aarau     int     2011
      198            Aarau     int     2012   1.756            Aarau     int     2012
      199            Aarau     int     2013   1.533            Aarau     int     2013
      200            Aarau     int     2014   2.601            Aarau     int     2014
      201            Aarau      ch     2011   1.332            Aarau      ch     2011
      202            Aarau      ch     2012   1.291            Aarau      ch     2012
      203            Aarau      ch     2013   1.311            Aarau      ch     2013
      204            Aarau      ch     2014   1.112            Aarau      ch     2014
      205            Aarau     int     2011   1.937            Aarau     int     2011
      206            Aarau     int     2012   1.756            Aarau     int     2012
      207            Aarau     int     2013   1.533            Aarau     int     2013
      208            Aarau     int     2014   2.601            Aarau     int     2014
      209            Aarau      ch     2011   1.332            Aarau      ch     2011
      210            Aarau      ch     2012   1.291            Aarau      ch     2012
      211            Aarau      ch     2013   1.311            Aarau      ch     2013
      212            Aarau      ch     2014   1.112            Aarau      ch     2014
      213            Aarau     int     2011   1.937            Aarau     int     2011
      214            Aarau     int     2012   1.756            Aarau     int     2012
      215            Aarau     int     2013   1.533            Aarau     int     2013
      216            Aarau     int     2014   2.601            Aarau     int     2014
      217            Aarau      ch     2011   1.332            Aarau      ch     2011
      218            Aarau      ch     2012   1.291            Aarau      ch     2012
      219            Aarau      ch     2013   1.311            Aarau      ch     2013
      220            Aarau      ch     2014   1.112            Aarau      ch     2014
      221            Aarau     int     2011   1.937            Aarau     int     2011
      222            Aarau     int     2012   1.756            Aarau     int     2012
      223            Aarau     int     2013   1.533            Aarau     int     2013
      224            Aarau     int     2014   2.601            Aarau     int     2014
      225            Aarau      ch     2011   1.332            Aarau      ch     2011
      226            Aarau      ch     2012   1.291            Aarau      ch     2012
      227            Aarau      ch     2013   1.311            Aarau      ch     2013
      228            Aarau      ch     2014   1.112            Aarau      ch     2014
      229            Aarau     int     2011   1.937            Aarau     int     2011
      230            Aarau     int     2012   1.756            Aarau     int     2012
      231            Aarau     int     2013   1.533            Aarau     int     2013
      232            Aarau     int     2014   2.601            Aarau     int     2014
      233            Aarau      ch     2011   1.332            Aarau      ch     2011
      234            Aarau      ch     2012   1.291            Aarau      ch     2012
      235            Aarau      ch     2013   1.311            Aarau      ch     2013
      236            Aarau      ch     2014   1.112            Aarau      ch     2014
      237            Aarau     int     2011   1.937            Aarau     int     2011
      238            Aarau     int     2012   1.756            Aarau     int     2012
      239            Aarau     int     2013   1.533            Aarau     int     2013
      240            Aarau     int     2014   2.601            Aarau     int     2014
      241            Aarau      ch     2011   1.332            Aarau      ch     2011
      242            Aarau      ch     2012   1.291            Aarau      ch     2012
      243            Aarau      ch     2013   1.311            Aarau      ch     2013
      244            Aarau      ch     2014   1.112            Aarau      ch     2014
      245            Aarau     int     2011   1.937            Aarau     int     2011
      246            Aarau     int     2012   1.756            Aarau     int     2012
      247            Aarau     int     2013   1.533            Aarau     int     2013
      248            Aarau     int     2014   2.601            Aarau     int     2014
      249            Aarau      ch     2011   1.332            Aarau      ch     2011
      250            Aarau      ch     2012   1.291            Aarau      ch     2012
      251            Aarau      ch     2013   1.311            Aarau      ch     2013
      252            Aarau      ch     2014   1.112            Aarau      ch     2014
      253            Aarau     int     2011   1.937            Aarau     int     2011
      254            Aarau     int     2012   1.756            Aarau     int     2012
      255            Aarau     int     2013   1.533            Aarau     int     2013
      256            Aarau     int     2014   2.601            Aarau     int     2014
      257            Aarau      ch     2011   1.332            Aarau      ch     2011
      258            Aarau      ch     2012   1.291            Aarau      ch     2012
      259            Aarau      ch     2013   1.311            Aarau      ch     2013
      260            Aarau      ch     2014   1.112            Aarau      ch     2014
      261            Aarau     int     2011   1.937            Aarau     int     2011
      262            Aarau     int     2012   1.756            Aarau     int     2012
      263            Aarau     int     2013   1.533            Aarau     int     2013
      264            Aarau     int     2014   2.601            Aarau     int     2014
      265            Aarau      ch     2011   1.332            Aarau      ch     2011
      266            Aarau      ch     2012   1.291            Aarau      ch     2012
      267            Aarau      ch     2013   1.311            Aarau      ch     2013
      268            Aarau      ch     2014   1.112            Aarau      ch     2014
      269            Aarau     int     2011   1.937            Aarau     int     2011
      270            Aarau     int     2012   1.756            Aarau     int     2012
      271            Aarau     int     2013   1.533            Aarau     int     2013
      272            Aarau     int     2014   2.601            Aarau     int     2014
      273            Aarau      ch     2011   1.332            Aarau      ch     2011
      274            Aarau      ch     2012   1.291            Aarau      ch     2012
      275            Aarau      ch     2013   1.311            Aarau      ch     2013
      276            Aarau      ch     2014   1.112            Aarau      ch     2014
      277            Aarau     int     2011   1.937            Aarau     int     2011
      278            Aarau     int     2012   1.756            Aarau     int     2012
      279            Aarau     int     2013   1.533            Aarau     int     2013
      280            Aarau     int     2014   2.601            Aarau     int     2014
          mab.mab fer.spatial_unit fer.nat fer.age fer.fer fer_y.spatial_unit
      1    32.732            Aarau      ch      15 0.00000              Aarau
      2    31.759            Aarau      ch      16 0.00000              Aarau
      3    33.181            Aarau      ch      17 0.00526              Aarau
      4    33.436            Aarau      ch      18 0.00483              Aarau
      5    29.559            Aarau      ch      19 0.00000              Aarau
      6    30.585            Aarau      ch      20 0.00920              Aarau
      7    30.480            Aarau      ch      21 0.00000              Aarau
      8    28.750            Aarau      ch      22 0.01005              Aarau
      9    32.732            Aarau      ch      23 0.00624              Aarau
      10   31.759            Aarau      ch      24 0.02442              Aarau
      11   33.181            Aarau      ch      25 0.01865              Aarau
      12   33.436            Aarau      ch      26 0.02103              Aarau
      13   29.559            Aarau      ch      27 0.05179              Aarau
      14   30.585            Aarau      ch      28 0.05508              Aarau
      15   30.480            Aarau      ch      29 0.06263              Aarau
      16   28.750            Aarau      ch      30 0.07253              Aarau
      17   32.732            Aarau      ch      31 0.09896              Aarau
      18   31.759            Aarau      ch      32 0.10676              Aarau
      19   33.181            Aarau      ch      33 0.11518              Aarau
      20   33.436            Aarau      ch      34 0.14846              Aarau
      21   29.559            Aarau      ch      35 0.08896              Aarau
      22   30.585            Aarau      ch      36 0.07634              Aarau
      23   30.480            Aarau      ch      37 0.07705              Aarau
      24   28.750            Aarau      ch      38 0.04403              Aarau
      25   32.732            Aarau      ch      39 0.05164              Aarau
      26   31.759            Aarau      ch      40 0.03002              Aarau
      27   33.181            Aarau      ch      41 0.01245              Aarau
      28   33.436            Aarau      ch      42 0.02637              Aarau
      29   29.559            Aarau      ch      43 0.00342              Aarau
      30   30.585            Aarau      ch      44 0.00000              Aarau
      31   30.480            Aarau      ch      45 0.00325              Aarau
      32   28.750            Aarau      ch      46 0.00000              Aarau
      33   32.732            Aarau      ch      47 0.00000              Aarau
      34   31.759            Aarau      ch      48 0.00000              Aarau
      35   33.181            Aarau      ch      49 0.00000              Aarau
      36   33.436            Aarau     int      15 0.00000              Aarau
      37   29.559            Aarau     int      16 0.00000              Aarau
      38   30.585            Aarau     int      17 0.00000              Aarau
      39   30.480            Aarau     int      18 0.00000              Aarau
      40   28.750            Aarau     int      19 0.05556              Aarau
      41   32.732            Aarau     int      20 0.07692              Aarau
      42   31.759            Aarau     int      21 0.07692              Aarau
      43   33.181            Aarau     int      22 0.05172              Aarau
      44   33.436            Aarau     int      23 0.08696              Aarau
      45   29.559            Aarau     int      24 0.05517              Aarau
      46   30.585            Aarau     int      25 0.12195              Aarau
      47   30.480            Aarau     int      26 0.09677              Aarau
      48   28.750            Aarau     int      27 0.08621              Aarau
      49   32.732            Aarau     int      28 0.11029              Aarau
      50   31.759            Aarau     int      29 0.09740              Aarau
      51   33.181            Aarau     int      30 0.15225              Aarau
      52   33.436            Aarau     int      31 0.11371              Aarau
      53   29.559            Aarau     int      32 0.11728              Aarau
      54   30.585            Aarau     int      33 0.09494              Aarau
      55   30.480            Aarau     int      34 0.15287              Aarau
      56   28.750            Aarau     int      35 0.05993              Aarau
      57   32.732            Aarau     int      36 0.05128              Aarau
      58   31.759            Aarau     int      37 0.08219              Aarau
      59   33.181            Aarau     int      38 0.05882              Aarau
      60   33.436            Aarau     int      39 0.05063              Aarau
      61   29.559            Aarau     int      40 0.01600              Aarau
      62   30.585            Aarau     int      41 0.02632              Aarau
      63   30.480            Aarau     int      42 0.00851              Aarau
      64   28.750            Aarau     int      43 0.02521              Aarau
      65   32.732            Aarau     int      44 0.00000              Aarau
      66   31.759            Aarau     int      45 0.01087              Aarau
      67   33.181            Aarau     int      46 0.00000              Aarau
      68   33.436            Aarau     int      47 0.00000              Aarau
      69   29.559            Aarau     int      48 0.00000              Aarau
      70   30.585            Aarau     int      49 0.00000              Aarau
      71   30.480            Aarau      ch      15 0.00000              Aarau
      72   28.750            Aarau      ch      16 0.00000              Aarau
      73   32.732            Aarau      ch      17 0.00526              Aarau
      74   31.759            Aarau      ch      18 0.00483              Aarau
      75   33.181            Aarau      ch      19 0.00000              Aarau
      76   33.436            Aarau      ch      20 0.00920              Aarau
      77   29.559            Aarau      ch      21 0.00000              Aarau
      78   30.585            Aarau      ch      22 0.01005              Aarau
      79   30.480            Aarau      ch      23 0.00624              Aarau
      80   28.750            Aarau      ch      24 0.02442              Aarau
      81   32.732            Aarau      ch      25 0.01865              Aarau
      82   31.759            Aarau      ch      26 0.02103              Aarau
      83   33.181            Aarau      ch      27 0.05179              Aarau
      84   33.436            Aarau      ch      28 0.05508              Aarau
      85   29.559            Aarau      ch      29 0.06263              Aarau
      86   30.585            Aarau      ch      30 0.07253              Aarau
      87   30.480            Aarau      ch      31 0.09896              Aarau
      88   28.750            Aarau      ch      32 0.10676              Aarau
      89   32.732            Aarau      ch      33 0.11518              Aarau
      90   31.759            Aarau      ch      34 0.14846              Aarau
      91   33.181            Aarau      ch      35 0.08896              Aarau
      92   33.436            Aarau      ch      36 0.07634              Aarau
      93   29.559            Aarau      ch      37 0.07705              Aarau
      94   30.585            Aarau      ch      38 0.04403              Aarau
      95   30.480            Aarau      ch      39 0.05164              Aarau
      96   28.750            Aarau      ch      40 0.03002              Aarau
      97   32.732            Aarau      ch      41 0.01245              Aarau
      98   31.759            Aarau      ch      42 0.02637              Aarau
      99   33.181            Aarau      ch      43 0.00342              Aarau
      100  33.436            Aarau      ch      44 0.00000              Aarau
      101  29.559            Aarau      ch      45 0.00325              Aarau
      102  30.585            Aarau      ch      46 0.00000              Aarau
      103  30.480            Aarau      ch      47 0.00000              Aarau
      104  28.750            Aarau      ch      48 0.00000              Aarau
      105  32.732            Aarau      ch      49 0.00000              Aarau
      106  31.759            Aarau     int      15 0.00000              Aarau
      107  33.181            Aarau     int      16 0.00000              Aarau
      108  33.436            Aarau     int      17 0.00000              Aarau
      109  29.559            Aarau     int      18 0.00000              Aarau
      110  30.585            Aarau     int      19 0.05556              Aarau
      111  30.480            Aarau     int      20 0.07692              Aarau
      112  28.750            Aarau     int      21 0.07692              Aarau
      113  32.732            Aarau     int      22 0.05172              Aarau
      114  31.759            Aarau     int      23 0.08696              Aarau
      115  33.181            Aarau     int      24 0.05517              Aarau
      116  33.436            Aarau     int      25 0.12195              Aarau
      117  29.559            Aarau     int      26 0.09677              Aarau
      118  30.585            Aarau     int      27 0.08621              Aarau
      119  30.480            Aarau     int      28 0.11029              Aarau
      120  28.750            Aarau     int      29 0.09740              Aarau
      121  32.732            Aarau     int      30 0.15225              Aarau
      122  31.759            Aarau     int      31 0.11371              Aarau
      123  33.181            Aarau     int      32 0.11728              Aarau
      124  33.436            Aarau     int      33 0.09494              Aarau
      125  29.559            Aarau     int      34 0.15287              Aarau
      126  30.585            Aarau     int      35 0.05993              Aarau
      127  30.480            Aarau     int      36 0.05128              Aarau
      128  28.750            Aarau     int      37 0.08219              Aarau
      129  32.732            Aarau     int      38 0.05882              Aarau
      130  31.759            Aarau     int      39 0.05063              Aarau
      131  33.181            Aarau     int      40 0.01600              Aarau
      132  33.436            Aarau     int      41 0.02632              Aarau
      133  29.559            Aarau     int      42 0.00851              Aarau
      134  30.585            Aarau     int      43 0.02521              Aarau
      135  30.480            Aarau     int      44 0.00000              Aarau
      136  28.750            Aarau     int      45 0.01087              Aarau
      137  32.732            Aarau     int      46 0.00000              Aarau
      138  31.759            Aarau     int      47 0.00000              Aarau
      139  33.181            Aarau     int      48 0.00000              Aarau
      140  33.436            Aarau     int      49 0.00000              Aarau
      141  29.559            Aarau      ch      15 0.00000              Aarau
      142  30.585            Aarau      ch      16 0.00000              Aarau
      143  30.480            Aarau      ch      17 0.00526              Aarau
      144  28.750            Aarau      ch      18 0.00483              Aarau
      145  32.732            Aarau      ch      19 0.00000              Aarau
      146  31.759            Aarau      ch      20 0.00920              Aarau
      147  33.181            Aarau      ch      21 0.00000              Aarau
      148  33.436            Aarau      ch      22 0.01005              Aarau
      149  29.559            Aarau      ch      23 0.00624              Aarau
      150  30.585            Aarau      ch      24 0.02442              Aarau
      151  30.480            Aarau      ch      25 0.01865              Aarau
      152  28.750            Aarau      ch      26 0.02103              Aarau
      153  32.732            Aarau      ch      27 0.05179              Aarau
      154  31.759            Aarau      ch      28 0.05508              Aarau
      155  33.181            Aarau      ch      29 0.06263              Aarau
      156  33.436            Aarau      ch      30 0.07253              Aarau
      157  29.559            Aarau      ch      31 0.09896              Aarau
      158  30.585            Aarau      ch      32 0.10676              Aarau
      159  30.480            Aarau      ch      33 0.11518              Aarau
      160  28.750            Aarau      ch      34 0.14846              Aarau
      161  32.732            Aarau      ch      35 0.08896              Aarau
      162  31.759            Aarau      ch      36 0.07634              Aarau
      163  33.181            Aarau      ch      37 0.07705              Aarau
      164  33.436            Aarau      ch      38 0.04403              Aarau
      165  29.559            Aarau      ch      39 0.05164              Aarau
      166  30.585            Aarau      ch      40 0.03002              Aarau
      167  30.480            Aarau      ch      41 0.01245              Aarau
      168  28.750            Aarau      ch      42 0.02637              Aarau
      169  32.732            Aarau      ch      43 0.00342              Aarau
      170  31.759            Aarau      ch      44 0.00000              Aarau
      171  33.181            Aarau      ch      45 0.00325              Aarau
      172  33.436            Aarau      ch      46 0.00000              Aarau
      173  29.559            Aarau      ch      47 0.00000              Aarau
      174  30.585            Aarau      ch      48 0.00000              Aarau
      175  30.480            Aarau      ch      49 0.00000              Aarau
      176  28.750            Aarau     int      15 0.00000              Aarau
      177  32.732            Aarau     int      16 0.00000              Aarau
      178  31.759            Aarau     int      17 0.00000              Aarau
      179  33.181            Aarau     int      18 0.00000              Aarau
      180  33.436            Aarau     int      19 0.05556              Aarau
      181  29.559            Aarau     int      20 0.07692              Aarau
      182  30.585            Aarau     int      21 0.07692              Aarau
      183  30.480            Aarau     int      22 0.05172              Aarau
      184  28.750            Aarau     int      23 0.08696              Aarau
      185  32.732            Aarau     int      24 0.05517              Aarau
      186  31.759            Aarau     int      25 0.12195              Aarau
      187  33.181            Aarau     int      26 0.09677              Aarau
      188  33.436            Aarau     int      27 0.08621              Aarau
      189  29.559            Aarau     int      28 0.11029              Aarau
      190  30.585            Aarau     int      29 0.09740              Aarau
      191  30.480            Aarau     int      30 0.15225              Aarau
      192  28.750            Aarau     int      31 0.11371              Aarau
      193  32.732            Aarau     int      32 0.11728              Aarau
      194  31.759            Aarau     int      33 0.09494              Aarau
      195  33.181            Aarau     int      34 0.15287              Aarau
      196  33.436            Aarau     int      35 0.05993              Aarau
      197  29.559            Aarau     int      36 0.05128              Aarau
      198  30.585            Aarau     int      37 0.08219              Aarau
      199  30.480            Aarau     int      38 0.05882              Aarau
      200  28.750            Aarau     int      39 0.05063              Aarau
      201  32.732            Aarau     int      40 0.01600              Aarau
      202  31.759            Aarau     int      41 0.02632              Aarau
      203  33.181            Aarau     int      42 0.00851              Aarau
      204  33.436            Aarau     int      43 0.02521              Aarau
      205  29.559            Aarau     int      44 0.00000              Aarau
      206  30.585            Aarau     int      45 0.01087              Aarau
      207  30.480            Aarau     int      46 0.00000              Aarau
      208  28.750            Aarau     int      47 0.00000              Aarau
      209  32.732            Aarau     int      48 0.00000              Aarau
      210  31.759            Aarau     int      49 0.00000              Aarau
      211  33.181            Aarau      ch      15 0.00000              Aarau
      212  33.436            Aarau      ch      16 0.00000              Aarau
      213  29.559            Aarau      ch      17 0.00526              Aarau
      214  30.585            Aarau      ch      18 0.00483              Aarau
      215  30.480            Aarau      ch      19 0.00000              Aarau
      216  28.750            Aarau      ch      20 0.00920              Aarau
      217  32.732            Aarau      ch      21 0.00000              Aarau
      218  31.759            Aarau      ch      22 0.01005              Aarau
      219  33.181            Aarau      ch      23 0.00624              Aarau
      220  33.436            Aarau      ch      24 0.02442              Aarau
      221  29.559            Aarau      ch      25 0.01865              Aarau
      222  30.585            Aarau      ch      26 0.02103              Aarau
      223  30.480            Aarau      ch      27 0.05179              Aarau
      224  28.750            Aarau      ch      28 0.05508              Aarau
      225  32.732            Aarau      ch      29 0.06263              Aarau
      226  31.759            Aarau      ch      30 0.07253              Aarau
      227  33.181            Aarau      ch      31 0.09896              Aarau
      228  33.436            Aarau      ch      32 0.10676              Aarau
      229  29.559            Aarau      ch      33 0.11518              Aarau
      230  30.585            Aarau      ch      34 0.14846              Aarau
      231  30.480            Aarau      ch      35 0.08896              Aarau
      232  28.750            Aarau      ch      36 0.07634              Aarau
      233  32.732            Aarau      ch      37 0.07705              Aarau
      234  31.759            Aarau      ch      38 0.04403              Aarau
      235  33.181            Aarau      ch      39 0.05164              Aarau
      236  33.436            Aarau      ch      40 0.03002              Aarau
      237  29.559            Aarau      ch      41 0.01245              Aarau
      238  30.585            Aarau      ch      42 0.02637              Aarau
      239  30.480            Aarau      ch      43 0.00342              Aarau
      240  28.750            Aarau      ch      44 0.00000              Aarau
      241  32.732            Aarau      ch      45 0.00325              Aarau
      242  31.759            Aarau      ch      46 0.00000              Aarau
      243  33.181            Aarau      ch      47 0.00000              Aarau
      244  33.436            Aarau      ch      48 0.00000              Aarau
      245  29.559            Aarau      ch      49 0.00000              Aarau
      246  30.585            Aarau     int      15 0.00000              Aarau
      247  30.480            Aarau     int      16 0.00000              Aarau
      248  28.750            Aarau     int      17 0.00000              Aarau
      249  32.732            Aarau     int      18 0.00000              Aarau
      250  31.759            Aarau     int      19 0.05556              Aarau
      251  33.181            Aarau     int      20 0.07692              Aarau
      252  33.436            Aarau     int      21 0.07692              Aarau
      253  29.559            Aarau     int      22 0.05172              Aarau
      254  30.585            Aarau     int      23 0.08696              Aarau
      255  30.480            Aarau     int      24 0.05517              Aarau
      256  28.750            Aarau     int      25 0.12195              Aarau
      257  32.732            Aarau     int      26 0.09677              Aarau
      258  31.759            Aarau     int      27 0.08621              Aarau
      259  33.181            Aarau     int      28 0.11029              Aarau
      260  33.436            Aarau     int      29 0.09740              Aarau
      261  29.559            Aarau     int      30 0.15225              Aarau
      262  30.585            Aarau     int      31 0.11371              Aarau
      263  30.480            Aarau     int      32 0.11728              Aarau
      264  28.750            Aarau     int      33 0.09494              Aarau
      265  32.732            Aarau     int      34 0.15287              Aarau
      266  31.759            Aarau     int      35 0.05993              Aarau
      267  33.181            Aarau     int      36 0.05128              Aarau
      268  33.436            Aarau     int      37 0.08219              Aarau
      269  29.559            Aarau     int      38 0.05882              Aarau
      270  30.585            Aarau     int      39 0.05063              Aarau
      271  30.480            Aarau     int      40 0.01600              Aarau
      272  28.750            Aarau     int      41 0.02632              Aarau
      273  32.732            Aarau     int      42 0.00851              Aarau
      274  31.759            Aarau     int      43 0.02521              Aarau
      275  33.181            Aarau     int      44 0.00000              Aarau
      276  33.436            Aarau     int      45 0.01087              Aarau
      277  29.559            Aarau     int      46 0.00000              Aarau
      278  30.585            Aarau     int      47 0.00000              Aarau
      279  30.480            Aarau     int      48 0.00000              Aarau
      280  28.750            Aarau     int      49 0.00000              Aarau
          fer_y.nat fer_y.year fer_y.age fer_y.birth_rate
      1          ch       2011        15      0.000000000
      2          ch       2011        16      0.000000000
      3          ch       2011        17      0.000000000
      4          ch       2011        18      0.000000000
      5          ch       2011        19      0.000000000
      6          ch       2011        20      0.000000000
      7          ch       2011        21      0.000000000
      8          ch       2011        22      0.025316456
      9          ch       2011        23      0.009302326
      10         ch       2011        24      0.015444015
      11         ch       2011        25      0.058608059
      12         ch       2011        26      0.069444444
      13         ch       2011        27      0.034582133
      14         ch       2011        28      0.029411765
      15         ch       2011        29      0.039867110
      16         ch       2011        30      0.098113208
      17         ch       2011        31      0.089068826
      18         ch       2011        32      0.110638298
      19         ch       2011        33      0.091666667
      20         ch       2011        34      0.176165803
      21         ch       2011        35      0.090497738
      22         ch       2011        36      0.161616162
      23         ch       2011        37      0.066666667
      24         ch       2011        38      0.075268817
      25         ch       2011        39      0.011904762
      26         ch       2011        40      0.012269939
      27         ch       2011        41      0.047846890
      28         ch       2011        42      0.000000000
      29         ch       2011        43      0.018018018
      30         ch       2011        44      0.000000000
      31         ch       2011        45      0.000000000
      32         ch       2011        46      0.000000000
      33         ch       2011        47      0.000000000
      34         ch       2011        48      0.000000000
      35         ch       2011        49      0.000000000
      36        int       2011        15      0.000000000
      37        int       2011        16      0.000000000
      38        int       2011        17      0.000000000
      39        int       2011        18      0.083333333
      40        int       2011        19      0.000000000
      41        int       2011        20      0.000000000
      42        int       2011        21      0.051282051
      43        int       2011        22      0.000000000
      44        int       2011        23      0.076923077
      45        int       2011        24      0.156862745
      46        int       2011        25      0.233333333
      47        int       2011        26      0.062500000
      48        int       2011        27      0.086021505
      49        int       2011        28      0.137254902
      50        int       2011        29      0.067415730
      51        int       2011        30      0.150000000
      52        int       2011        31      0.080645161
      53        int       2011        32      0.162790698
      54        int       2011        33      0.064516129
      55        int       2011        34      0.051948052
      56        int       2011        35      0.061855670
      57        int       2011        36      0.184615385
      58        int       2011        37      0.083333333
      59        int       2011        38      0.065217391
      60        int       2011        39      0.024096386
      61        int       2011        40      0.028571429
      62        int       2011        41      0.024691358
      63        int       2011        42      0.000000000
      64        int       2011        43      0.000000000
      65        int       2011        44      0.000000000
      66        int       2011        45      0.000000000
      67        int       2011        46      0.000000000
      68        int       2011        47      0.000000000
      69        int       2011        48      0.000000000
      70        int       2011        49      0.000000000
      71         ch       2012        15      0.000000000
      72         ch       2012        16      0.000000000
      73         ch       2012        17      0.000000000
      74         ch       2012        18      0.015503876
      75         ch       2012        19      0.000000000
      76         ch       2012        20      0.000000000
      77         ch       2012        21      0.000000000
      78         ch       2012        22      0.029850746
      79         ch       2012        23      0.021978022
      80         ch       2012        24      0.046153846
      81         ch       2012        25      0.020270270
      82         ch       2012        26      0.013745704
      83         ch       2012        27      0.065868263
      84         ch       2012        28      0.061797753
      85         ch       2012        29      0.100719424
      86         ch       2012        30      0.065146580
      87         ch       2012        31      0.108695652
      88         ch       2012        32      0.135458167
      89         ch       2012        33      0.131578947
      90         ch       2012        34      0.155102041
      91         ch       2012        35      0.085106383
      92         ch       2012        36      0.035087719
      93         ch       2012        37      0.060913706
      94         ch       2012        38      0.057416268
      95         ch       2012        39      0.045977011
      96         ch       2012        40      0.011834320
      97         ch       2012        41      0.012903226
      98         ch       2012        42      0.009433962
      99         ch       2012        43      0.000000000
      100        ch       2012        44      0.000000000
      101        ch       2012        45      0.000000000
      102        ch       2012        46      0.000000000
      103        ch       2012        47      0.000000000
      104        ch       2012        48      0.000000000
      105        ch       2012        49      0.000000000
      106       int       2012        15      0.000000000
      107       int       2012        16      0.000000000
      108       int       2012        17      0.000000000
      109       int       2012        18      0.000000000
      110       int       2012        19      0.090909091
      111       int       2012        20      0.066666667
      112       int       2012        21      0.071428571
      113       int       2012        22      0.000000000
      114       int       2012        23      0.000000000
      115       int       2012        24      0.071428571
      116       int       2012        25      0.125000000
      117       int       2012        26      0.033333333
      118       int       2012        27      0.060000000
      119       int       2012        28      0.080000000
      120       int       2012        29      0.115384615
      121       int       2012        30      0.044444444
      122       int       2012        31      0.105263158
      123       int       2012        32      0.105263158
      124       int       2012        33      0.109890110
      125       int       2012        34      0.263736264
      126       int       2012        35      0.114285714
      127       int       2012        36      0.000000000
      128       int       2012        37      0.067796610
      129       int       2012        38      0.107526882
      130       int       2012        39      0.043010753
      131       int       2012        40      0.000000000
      132       int       2012        41      0.028985507
      133       int       2012        42      0.000000000
      134       int       2012        43      0.051282051
      135       int       2012        44      0.000000000
      136       int       2012        45      0.000000000
      137       int       2012        46      0.000000000
      138       int       2012        47      0.000000000
      139       int       2012        48      0.000000000
      140       int       2012        49      0.000000000
      141        ch       2013        15      0.000000000
      142        ch       2013        16      0.000000000
      143        ch       2013        17      0.013513514
      144        ch       2013        18      0.000000000
      145        ch       2013        19      0.000000000
      146        ch       2013        20      0.029411765
      147        ch       2013        21      0.000000000
      148        ch       2013        22      0.000000000
      149        ch       2013        23      0.000000000
      150        ch       2013        24      0.028436019
      151        ch       2013        25      0.019867550
      152        ch       2013        26      0.030211480
      153        ch       2013        27      0.056074766
      154        ch       2013        28      0.050279330
      155        ch       2013        29      0.053892216
      156        ch       2013        30      0.076124567
      157        ch       2013        31      0.071895425
      158        ch       2013        32      0.111498258
      159        ch       2013        33      0.102362205
      160        ch       2013        34      0.127272727
      161        ch       2013        35      0.066115702
      162        ch       2013        36      0.127659574
      163        ch       2013        37      0.103004292
      164        ch       2013        38      0.050251256
      165        ch       2013        39      0.057971014
      166        ch       2013        40      0.074074074
      167        ch       2013        41      0.000000000
      168        ch       2013        42      0.051948052
      169        ch       2013        43      0.000000000
      170        ch       2013        44      0.000000000
      171        ch       2013        45      0.009090909
      172        ch       2013        46      0.000000000
      173        ch       2013        47      0.000000000
      174        ch       2013        48      0.000000000
      175        ch       2013        49      0.000000000
      176       int       2013        15      0.000000000
      177       int       2013        16      0.000000000
      178       int       2013        17      0.000000000
      179       int       2013        18      0.000000000
      180       int       2013        19      0.000000000
      181       int       2013        20      0.000000000
      182       int       2013        21      0.000000000
      183       int       2013        22      0.000000000
      184       int       2013        23      0.078431373
      185       int       2013        24      0.060606061
      186       int       2013        25      0.142857143
      187       int       2013        26      0.086956522
      188       int       2013        27      0.066666667
      189       int       2013        28      0.116504854
      190       int       2013        29      0.123711340
      191       int       2013        30      0.134615385
      192       int       2013        31      0.131868132
      193       int       2013        32      0.123711340
      194       int       2013        33      0.089552239
      195       int       2013        34      0.061224490
      196       int       2013        35      0.000000000
      197       int       2013        36      0.140845070
      198       int       2013        37      0.043956044
      199       int       2013        38      0.036363636
      200       int       2013        39      0.046511628
      201       int       2013        40      0.022727273
      202       int       2013        41      0.025974026
      203       int       2013        42      0.000000000
      204       int       2013        43      0.000000000
      205       int       2013        44      0.000000000
      206       int       2013        45      0.000000000
      207       int       2013        46      0.000000000
      208       int       2013        47      0.000000000
      209       int       2013        48      0.000000000
      210       int       2013        49      0.000000000
      211        ch       2014        15      0.000000000
      212        ch       2014        16      0.000000000
      213        ch       2014        17      0.000000000
      214        ch       2014        18      0.000000000
      215        ch       2014        19      0.000000000
      216        ch       2014        20      0.000000000
      217        ch       2014        21      0.000000000
      218        ch       2014        22      0.000000000
      219        ch       2014        23      0.000000000
      220        ch       2014        24      0.000000000
      221        ch       2014        25      0.015384615
      222        ch       2014        26      0.018237082
      223        ch       2014        27      0.034383954
      224        ch       2014        28      0.053097345
      225        ch       2014        29      0.040462428
      226        ch       2014        30      0.076433121
      227        ch       2014        31      0.118466899
      228        ch       2014        32      0.078688525
      229        ch       2014        33      0.113475177
      230        ch       2014        34      0.160642570
      231        ch       2014        35      0.117117117
      232        ch       2014        36      0.075313808
      233        ch       2014        37      0.062176166
      234        ch       2014        38      0.026315789
      235        ch       2014        39      0.050000000
      236        ch       2014        40      0.009900990
      237        ch       2014        41      0.025000000
      238        ch       2014        42      0.024242424
      239        ch       2014        43      0.012578616
      240        ch       2014        44      0.000000000
      241        ch       2014        45      0.000000000
      242        ch       2014        46      0.000000000
      243        ch       2014        47      0.000000000
      244        ch       2014        48      0.000000000
      245        ch       2014        49      0.000000000
      246       int       2014        15      0.000000000
      247       int       2014        16      0.000000000
      248       int       2014        17      0.000000000
      249       int       2014        18      0.000000000
      250       int       2014        19      0.074074074
      251       int       2014        20      0.129032258
      252       int       2014        21      0.210526316
      253       int       2014        22      0.157894737
      254       int       2014        23      0.162162162
      255       int       2014        24      0.035714286
      256       int       2014        25      0.090909091
      257       int       2014        26      0.175438596
      258       int       2014        27      0.138888889
      259       int       2014        28      0.144927536
      260       int       2014        29      0.056074766
      261       int       2014        30      0.273684211
      262       int       2014        31      0.106194690
      263       int       2014        32      0.127659574
      264       int       2014        33      0.087912088
      265       int       2014        34      0.144000000
      266       int       2014        35      0.078431373
      267       int       2014        36      0.037037037
      268       int       2014        37      0.144927536
      269       int       2014        38      0.022222222
      270       int       2014        39      0.068965517
      271       int       2014        40      0.025000000
      272       int       2014        41      0.024390244
      273       int       2014        42      0.027397260
      274       int       2014        43      0.027027027
      275       int       2014        44      0.000000000
      276       int       2014        45      0.030303030
      277       int       2014        46      0.000000000
      278       int       2014        47      0.000000000
      279       int       2014        48      0.000000000
      280       int       2014        49      0.000000000

