extensions[ array csv time ]

; __includes["distribution-utils.nls"]

globals [
  ; input variables:
  precip inflow outflow water-use evap losses

  ; actual data:
  act-precip act-inflow act-evap act-water-use act-storage-mem act-losses
  variable-table act-outflow avg-obs-storage

  ; modeled variables:
  mod-storage-mem mod-storage prev-storage prev-losses

  ; for time-keeping:
  tick-month month

  ; for scenarios and archetypes
  arch-prob

  ; for reading in different climate scenarios data
  climate-data-var-name
]

turtles-own [ archetype consumption ] ; each turtle is assigned an archetype + a water consumption value

to setup
  clear-all
  file-close
  reset-timer

  ; set timer to and count up by months from different starting points (dependent on climate-scenario)
  (ifelse climate-scenario = "hist" [ set tick-month time:anchor-to-ticks (time:create "1976-01-01") 1 "month" ]
    climate-scenario != "hist" [ set tick-month time:anchor-to-ticks (time:create "2006-01-01") 1 "month" ]
   )

  ; set climate data variable name
  set climate-data-var-name ( word city "_" climate-model "_" climate-scenario ".csv" )

  ; set input variables based on city selection
  set variable-table csv:from-file ( word "./GCMdata/" climate-data-var-name )

  set act-storage-mem []
  set mod-storage-mem []

  ; create turtles based on city selection (using 2021 population)
  (ifelse city = "Denver" [
    create-turtles 711463 [
    set arch-prob random-float 100.0 ; set archetype probability
    assign-archetypes
    ] ]
    city = "LasVegas" [
    create-turtles 646790 [
    set arch-prob random-float 100.0 ; set archetype probability
    assign-archetypes
    ] ]
    city = "Phoenix" [
    create-turtles 1625000 [
    set arch-prob random-float 100.0 ; set archetype probability
    assign-archetypes
    ] ]
  )

  print "Ready!"
  reset-ticks
end

to go
  set month time:get "month" tick-month ; find the month, which will be used to determine the season
  get-input-variables
  daily-storage

  ; set up stopping conditions
  (ifelse climate-scenario = "hist" [
    if (ticks = 359) [
    show (word "Model finished in " timer " seconds")
    stop
    ] ]
    climate-scenario != "hist" [
    if (ticks = 1122) [
    show (word "Model finished in " timer " seconds")
    stop
    ] ]
  )
  tick
end

to get-input-variables
  let variable-list item ( ticks + 1 ) variable-table
  (ifelse city = "Denver" or city = "LasVegas" [
    set act-precip item 2 variable-list     ; actual preciptiation
    set act-evap item 3 variable-list       ; actual evaporation
    set act-inflow item 4 variable-list     ; actual inflow
    set act-outflow item 5 variable-list    ; actual outflow
    ]
    city = "Phoenix" [
    set act-precip item 2 variable-list     ; actual preciptiation
    set act-evap item 3 variable-list       ; actual evaporation
    set act-inflow item 4 variable-list     ; actual inflow
    ]
   )

  ; set observed average storage
  (ifelse city = "Denver" [ set avg-obs-storage 729734232.1 ]
          city = "LasVegas" [ set avg-obs-storage 14127723770 ]
          city = "Phoenix" [ set avg-obs-storage 1729627532 ]
  )

  (ifelse ticks = 0 [ set prev-storage avg-obs-storage ] )

  ; set input variables
  set precip act-precip
  set inflow act-inflow
  set outflow act-outflow
  set evap act-evap

  ; determine the miscellaneous loss term
  (ifelse ticks = 0 [ set prev-losses 0 ]
    ticks > 0 and city = "Denver" [ set prev-losses ( prev-storage - mod-storage + precip + inflow - water-use - evap - outflow ) ]
    ticks > 0 and city = "LasVegas" [ set prev-losses ( prev-storage - mod-storage + precip + ( inflow + ( water-use * 0.4 ) ) - water-use - evap - outflow ) ]
    ticks > 0 and city = "Phoenix" [ set prev-losses ( prev-storage - mod-storage + precip + inflow - water-use - evap ) ]
  )

  set losses prev-losses * random-normal 1 0.5

  ; turtles consume water
  ask turtles [
    (ifelse month = 12 or month = 1 or month = 2 or month = 3 [ use-water-winter ]  ; call winter water use distributions
      month = 4 or month = 5 or month = 10 or month = 11 [ use-water-intermediate ] ; call intermediate water use distributions
      month = 6 or month = 7 or month = 8 or month = 9 [ use-water-summer ]         ; call summer water distribution
    )
  ]

  ; calculate total water consumption
  ( ifelse adaptive-water-use
      [ (ifelse mod-storage > ( avg-obs-storage * 0.75 ) [ set water-use ( sum [ consumption ] of turtles ) ] ; business-as-usual
                mod-storage < ( avg-obs-storage * 0.75 ) and mod-storage > ( avg-obs-storage * 0.5 ) [ set water-use ( sum [ consumption ] of turtles * 0.95 ) ] ; 5% reduction under "moderate drought"
                mod-storage < ( avg-obs-storage * 0.5 ) and mod-storage > ( avg-obs-storage * 0.25 ) [ set water-use ( sum [ consumption ] of turtles * 0.90 ) ] ; 10% reduction under "severe drought"
                mod-storage < ( avg-obs-storage * 0.25 ) [ set water-use ( sum [ consumption ] of turtles * 0.80 ) ] ; 20% reduction under "extreme drought"
        )
      ]
      [ set water-use ( sum [ consumption ] of turtles ) ]
  )
end

to use-water-winter
  (ifelse city = "Denver" [
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 [ set consumption random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.82578295 0.07076726 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 0.5949670 0.0430368 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.82578295 0.07076726 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.82578295 0.07076726 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 [ set consumption random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.82578295 0.07076726 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 [ set consumption random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption random-gamma 1.82578295 0.07076726 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 0.69383487 0.03830158 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.44593670 0.05171909 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 0.5949670 0.0430368 ]
      archetype = 4 [ set consumption random-gamma 0.7068694 0.0439146 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.53100050 0.04558886 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.54812029 0.04581188 ]
      archetype = 7 [ set consumption random-gamma 1.82578295 0.07076726 ]
    ) ]
    ) ]
  city = "LasVegas" [
      (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 [ set consumption random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma 3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.91393261 0.04489809 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 20.8444547 0.6650227 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 1.96829297 0.03952231 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma 3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.91393261 0.04489809 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 20.8444547 0.6650227 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma 3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.91393261 0.04489809 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 20.8444547 0.6650227 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 [ set consumption random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma 3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 8.4894976 0.6166054 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 20.8444547 0.6650227 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 [ set consumption random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma 3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.91393261 0.04489809 ]
      archetype = 7 [ set consumption random-gamma 20.8444547 0.6650227 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 1.75415349 0.04079442 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 3.61536022 0.04973653 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.96829297 0.03952231 ]
      archetype = 4 [ set consumption random-gamma  3.0671535 0.0676422 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.23974107 0.04566462  ]
      archetype = 6 [ set consumption 0.95 * random-gamma 2.91393261 0.04489809 ]
      archetype = 7 [ set consumption random-gamma 20.8444547 0.6650227 ]
    ) ]
  ) ]
  city = "Phoenix" [
      (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 [ set consumption random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.1818585 0.1003551 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 1.6702572 0.0539725 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.1818585 0.1003551 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.1818585 0.1003551 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 [ set consumption random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.1818585 0.1003551 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 [ set consumption random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption random-gamma 3.1818585 0.1003551 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 1.74126929 0.06104617 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.6865010 0.0524653 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.6702572 0.0539725 ]
      archetype = 4 [ set consumption random-gamma 1.89908045 0.06480985 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.072876 0.077330 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.9255952 0.1257295 ]
      archetype = 7 [ set consumption random-gamma 3.1818585 0.1003551 ]
    ) ]
  ) ]
  )
end

to use-water-intermediate
  (ifelse city = "Denver" [
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 [ set consumption random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.44724700 0.06626899 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 0.9367082 0.0419970 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.44724700 0.06626899 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.44724700 0.06626899 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 [ set consumption random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.44724700 0.06626899 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 [ set consumption random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption random-gamma 2.44724700 0.06626899 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 0.91361689 0.03497943 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 0.78470791 0.04835234 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 0.9367082 0.0419970 ]
      archetype = 4 [ set consumption random-gamma 1.07528238 0.04280772 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 0.76647942 0.04044039 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.72915881 0.03854907 ]
      archetype = 7 [ set consumption random-gamma 2.44724700 0.06626899 ]
    ) ]
    ) ]
   city = "LasVegas" [
      (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 [ set consumption random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 45.452167 1.078854 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 2.93200345 0.04839236 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 45.452167 1.078854 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 45.452167 1.078854 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 [ set consumption random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 45.452167 1.078854 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 [ set consumption random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption random-gamma 45.452167 1.078854 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 2.76575427 0.05142154 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 4.79439714 0.05743422 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.93200345 0.04839236 ]
      archetype = 4 [ set consumption random-gamma 4.75724343 0.08475424 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.35840882 0.05612905 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.98524389 0.05265419 ]
      archetype = 7 [ set consumption random-gamma 45.452167 1.078854 ]
    ) ]
  ) ]
    city = "Phoenix" [
      (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 [ set consumption random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 4.2565000 0.1163738 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 2.24374124 0.06264542 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 4.2565000 0.1163738 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 4.2565000 0.1163738 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 [ set consumption random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 4.2565000 0.1163738 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 [ set consumption random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption random-gamma 4.2565000 0.1163738 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 2.39503581 0.07172078 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.24223424 0.07362998 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.24374124 0.06264542 ]
      archetype = 4 [ set consumption random-gamma 2.59219994 0.07585649 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.90826487 0.09181385 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 5.2806056 0.1463071 ]
      archetype = 7 [ set consumption random-gamma 4.2565000 0.1163738 ]
    ) ]
  ) ]
  )
end

to use-water-summer
  (ifelse city = "Denver" [
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 [ set consumption random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.40476588 0.06819625 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 1.35108675 0.04145543 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.40476588 0.06819625 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.40476588 0.06819625 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 [ set consumption random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.40476588 0.06819625 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 [ set consumption random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption random-gamma 3.40476588 0.06819625 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 1.22791461 0.03404721 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.13723568 0.04465191 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.35108675 0.04145543 ]
      archetype = 4 [ set consumption random-gamma 1.37895177 0.03933942 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.03259574 0.03708803 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 0.85142931 0.03210057 ]
      archetype = 7 [ set consumption random-gamma 3.40476588 0.06819625 ]
    ) ]
    ) ]
    city = "LasVegas" [
       (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 [ set consumption random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 75.698495 1.382654 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 4.27860908 0.05844551 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 75.698495 1.382654 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 75.698495 1.382654 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 [ set consumption random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 75.698495 1.382654 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 [ set consumption random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption random-gamma 75.698495 1.382654 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 4.21352663 0.06345236 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 6.3510514 0.0660914 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 4.27860908 0.05844551 ]
      archetype = 4 [ set consumption random-gamma 7.1307652 0.1037225 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 4.92180533 0.06793161 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 5.42280773 0.06140944 ]
      archetype = 7 [ set consumption random-gamma 75.698495 1.382654 ]
    ) ]
  ) ]
    city = "Phoenix" [
    (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 [ set consumption random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 5.1101465 0.1276804 ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 2.70033134 0.06877479 ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 5.1101465 0.1276804 ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 5.1101465 0.1276804 ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 [ set consumption random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption 1.15 * random-gamma 5.1101465 0.1276804 ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 [ set consumption random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 1.25 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption random-gamma 5.1101465 0.1276804 ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 2.92022051 0.07926605 ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.68294411 0.06630772 ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.70033134 0.06877479 ]
      archetype = 4 [ set consumption random-gamma 3.14734926 0.08366319 ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.5843295 0.1020521 ]
      archetype = 6 [ set consumption 0.95 * random-gamma 6.3589228 0.1608246 ]
      archetype = 7 [ set consumption random-gamma 5.1101465 0.1276804 ]
    ) ]
  ) ]
    )
end

to assign-archetypes ; set archetype based on previously determined proportions (Obringer and White, 2023)
  (ifelse city = "Denver" [
        (ifelse arch-prob <= 13.0 [ set archetype 1 ]
          arch-prob > 13.0 and arch-prob <= 43.4 [ set archetype 2 ]
          arch-prob > 43.4 and arch-prob <= 71.1 [ set archetype 3 ]
          arch-prob > 71.1 and arch-prob <= 79.8 [ set archetype 4 ]
          arch-prob > 79.8 and arch-prob <= 94.8 [ set archetype 5 ]
          arch-prob > 94.8 and arch-prob <= 97.2 [ set archetype 6 ]
          arch-prob > 97.2 [ set archetype 7 ]
    ) ]
    city = "LasVegas" [
    (ifelse arch-prob <= 17.0 [ set archetype 1 ]
          arch-prob > 13.0 and arch-prob <= 31.7 [ set archetype 2 ]
          arch-prob > 31.7 and arch-prob <= 59.4 [ set archetype 3 ]
          arch-prob > 59.4 and arch-prob <= 66.5 [ set archetype 4 ]
          arch-prob > 66.5 and arch-prob <= 94.6 [ set archetype 5 ]
          arch-prob > 94.6 and arch-prob <= 98.6 [ set archetype 6 ]
          arch-prob > 98.6 [ set archetype 7 ]
        ) ]
    city = "Phoenix" [
    (ifelse arch-prob <= 19.4 [ set archetype 1 ]
          arch-prob > 19.4 and arch-prob <= 38.5 [ set archetype 2 ]
          arch-prob > 38.5 and arch-prob <= 63.1 [ set archetype 3 ]
          arch-prob > 63.1 and arch-prob <= 76.4 [ set archetype 4 ]
          arch-prob > 76.4 and arch-prob <= 94.8 [ set archetype 5 ]
          arch-prob > 94.8 and arch-prob <= 98.7 [ set archetype 6 ]
          arch-prob > 98.7 [ set archetype 7 ]
        ) ]
    )
end

to daily-storage
  (ifelse city = "Denver" [ set mod-storage (prev-storage + precip + inflow - water-use - evap - outflow - losses ) ]
    city = "LasVegas" [ set mod-storage (prev-storage + precip + ( inflow + ( water-use * 0.4 ) ) - water-use - evap - outflow - losses ) ] ; add 40% water use to inflow for LV recycling
    city = "Phoenix" [ set mod-storage (prev-storage + precip + inflow - water-use - evap - losses ) ]
    )
  set prev-storage mod-storage
end
@#$#@#$#@
GRAPHICS-WINDOW
10
14
474
378
-1
-1
2.52
1
10
1
1
1
0
1
1
1
-90
90
-70
70
0
0
1
ticks
30.0

BUTTON
566
83
638
116
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
567
33
637
66
SETUP
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
950
10
1100
30
INPUTS
16
0.0
1

TEXTBOX
947
389
1097
409
OUTPUTS
16
0.0
1

PLOT
669
412
1316
603
storage
time
storage
0.0
10.0
0.0
4.0
true
true
"" ""
PENS
"Model" 1.0 0 -5298144 true "" "plot mod-storage"

PLOT
668
34
868
184
precip
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -11221820 true "" "plot precip"
"pen-1" 1.0 0 -7500403 true "" "plot act-precip"

PLOT
887
35
1087
185
inflow
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -14439633 true "" "plot inflow"
"pen-1" 1.0 0 -7500403 true "" "plot act-inflow"

PLOT
1114
35
1314
185
evap
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -4079321 true "" "plot evap"
"pen-1" 1.0 0 -7500403 true "" "plot act-evap"

PLOT
674
210
874
360
wateruse
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13791810 true "" "plot water-use"
"pen-1" 1.0 0 -7500403 true "" "plot act-water-use"

CHOOSER
513
147
651
192
scenario
scenario
"base" "partial-part" "part" "indiv" "concern" "all-changes"
0

CHOOSER
515
213
653
258
city
city
"Denver" "LasVegas" "Phoenix"
0

CHOOSER
514
279
652
324
climate-scenario
climate-scenario
"hist" "rcp2p6" "rcp4p5" "rcp6p0" "rcp8p5"
0

CHOOSER
512
344
650
389
climate-model
climate-model
"gfdl" "hadgem" "ipsl" "miroc"
0

PLOT
892
210
1092
360
outflow
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot outflow"

PLOT
1119
211
1319
361
losses
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot losses"

SWITCH
496
399
652
432
adaptive-water-use
adaptive-water-use
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="climate-models" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mod-storage</metric>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;base&quot;"/>
      <value value="&quot;part&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="city">
      <value value="&quot;Phoenix&quot;"/>
      <value value="&quot;Denver&quot;"/>
      <value value="&quot;LasVegas&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-model">
      <value value="&quot;gfdl&quot;"/>
      <value value="&quot;hadgem&quot;"/>
      <value value="&quot;ipsl&quot;"/>
      <value value="&quot;miroc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="climate-scenario">
      <value value="&quot;hist&quot;"/>
      <value value="&quot;rcp2p6&quot;"/>
      <value value="&quot;rcp4p5&quot;"/>
      <value value="&quot;rcp6p0&quot;"/>
      <value value="&quot;rcp8p5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adaptive-water-use">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
