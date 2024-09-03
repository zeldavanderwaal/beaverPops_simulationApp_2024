
cat("about txt - ")
  
 ui_aboutTab_general_testing <- p(style="color:orange;font-style:italic;text-align:center;","..writing in in progress here..") 

    ui_aboutTab_general <- 

fluidRow(style="color: beige; padding: 0 5vw;",
         p(style="padding: 1vh 1vw 0 1vw;", "Beavers are an interesting, yet challenging species to model. Our 2020 study (prepared for NatureScot, in review) 
has allowed the update and validation of the Shirley ",span("et.al",style="font-style:italic;")," (2015) model by using repeated large scale field survey data to test and refine the assumptions of the model."), 
div(style="padding: 10px 1vw 7px 3vw;", "However as with any model there are caveats to consider, including:"),                          
         div(style="display:flex;margin: 0 3vw;background-color: #d1d58f1f;border-radius: 0 2vw 0 0;padding: 14px 14px 0 28px;",
             
             p(style="display:list-item;    margin: 0 14px;",span(style="font-weight:bold;display:block;","unobserved behaviours "),"
Some aspects of beaver biology and behaviour are not completely understood or documented, making it challenging to accurately reflect some behaviours in the model. 
For instance, dispersal and settlement behaviours (distance roamed, choice of settling location) may differ between individuals from established family groups or territories, partially disturbed territories (such as dam removal, incomplete removal of families from licensed areas) 
and translocated individuals (from an enclosure or a new location). 
"),
             
             
             p(style="display:list-item;    margin: 0 28px;",span(style="font-weight:bold;display:block;","local variability "),"
The life history parameters used in the model are those described in Shirley ",span("et al.", style="font-style:italic;")," (2015), which were drawn from a range of published sources from studies across Europe. Mortality rates, litter sizes, and territory sizes in particular are highly variable due to the different pressures applied to beaver populations, and could well be different in a local population and vary between a translocated population and an established population.
")
         ),
         
         div(style="display:flex; margin: 0 3vw 0 3vw;padding: 14px;background-color: #d1d58f1f;border-radius: 0 0 2vw 2vw;padding: 0 14px;",
             p(style="display:list-item;    margin: 28px 28px 14px 28px;",span(style="font-weight:bold;display:block;","habitat suitability map"),"
The distribution of simulated beavers is very much dependent on the habitat map (supplied by Exeter University, 2023). 
There may be a mismatch between locations of known (observed) beaver territories and the amount of available habitat estimated in the underlying habitat map used in the model. 
While the app allows users to suggest any location to establish an observed territory (via upload), the simulated beavers will not be able to settle at a given location unless the habitat requirements stipulated in the model are fully met. This mismatch in habitat quality may have an impact on the spatial model predictions."),
             
             
             p(style="display:list-item;    margin: 28px 28px 14px 28px;",span(style="font-weight:bold;display:block;","uncertainty through time "),"
The model used in this app is stochastic and contains uncertainties that originate from several sources, including: 
sightings data to develop the simulation model, 
population parameters (life history and movement), 
habitat suitability categorisation on the support map, 
local variations, behavioural responses.
These uncertainties are cumulative.
Through time, the environment may also be modified (by beavers as well as anthropogenic pressure) and the habitat map may not reflect the conditions of a future landscape. 
We therefore advise exercising caution in interpreting simulation output beyond 5 years as uncertainty will increase through time.")  
         ),
         br(),
         div(style="text-align: right;","The challenge is in asking the right questions of the model, to test a range of scenarios and be clear about all assumptions made and their limitations."),
         p(style="text-align: right;","Refer to ", span(class="aboutBtnTxt","Assumptions and Parameters",style="color:hotpink;border-color:hotpink;")," for more details about the model processes.")
)