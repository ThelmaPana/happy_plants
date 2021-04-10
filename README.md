# Happy plants

A shiny app to help you look after your plants.


## Features
Everyday you open the app, you know which plants you have to water and to feed, depending on the current season.
Plants in need of food or water are displayed in three lists:
- `overdue plants` plants which needed care in the previous days
- `today’s plants` plants which need care today
- `coming soon` plants which will need care in the three coming days

Check the plants you’ve watered or fed and click on `Done` to save.

## Getting started

1. Fill your plants info in a Google Sheet (see [example](https://docs.google.com/spreadsheets/d/e/2PACX-1vT94TwBeftIcMUZR28kI_g8ubjsLzU3MQCXLSyrbs79N6P-8Lxtmx8dYzogteUbAa9YZAbOFIRW91eU/pubhtml)) with mandatory columns:

    - `name` of your plant. Do not use the same name multiple times.
    - `room` where the plant is
    - `watered_last` date of last watering
    - `fed_last` date of last feeding
    - `water_freq_summer` watering frequency in days during summer
    - `water_freq_mid` watering frequency in days during fall and spring
    - `water_freq_winter` watering frequency in days during winter
    - `food_freq_summer` feeding frequency in days during summer
    - `food_freq_mid` feeding frequency in days during fall and spring
    - `food_freq_winter` feeding frequency in days during winter

You may add any other columns for extra info. 

2. Set up non interactive authentication in `gs4_auth.R` (see https://medium.com/@JosiahParry/googlesheets4-authentication-for-deployment-9e994b4c81d6).

3. Paste your spreadsheet URL in the `app.R` script

## A few remarks
- If your plants do not care about seasons or you live in a seasonless area, just set `XXX_freq_summer`, `XXX_freq_mid` and `XXX_freq_winter` to the same value.
- If your plant does not need water or food during winter, fill the corresponding cell with `/`.
- Season is guessed according to date for northern hemisphere but this is easily customisable.
- I use a Google Sheet with two headers for readability and sharing but you may as well use a local csv file.

