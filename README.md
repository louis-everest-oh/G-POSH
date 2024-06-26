# G-computation for Policy evaluation in Occupational Safety and Health (G-POSH)

Contains easy-to-use G-computation functions for occupational health research, to summarise potential impacts of policy alternatives on cause-specific mortality.

### Developlemt Notes
 This is a beta version of the GPOSH package 

## Getting Started
Install from Github 

    library(devtools) 
    install_github("louis-everest-oh/G-POSH")
    

## Functions

### Discrete Time Hazard Table

Formats cohort data into discrete time hazard data structures, to a specified time-point.

    dth_table(data, exp_prefix, event_dates, dlo_max = "none")

### Expected Risk of an Event of Interest

Calculates the probability of surviving for each person i and time interval u.

    expected_risks(data, hazards, id_in = "id", prefix = "")


## Authors

  - **Louis Everest** - [aut,cre]

See also the list of
[contributors](https://github.com/PurpleBooth/a-good-readme-template/contributors)
who participated in this project.

## License

This project is licensed under GPL-3 

## Acknowledgments

  - The G-POSH package is based on the approached described by Richardson et al. (2020) ‘Innovations in applied decision theory for health
and safety’ 
