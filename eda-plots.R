

ggplot(surveys, aes(x=coronavirusConcern)) + geom_histogram()

ggplot(surveys, aes(x=birthYear2020, y=coronavirusConcern)) + stat_smooth() #older more concerned\



ggplot(surveys, aes(x=politicalBelief, y=coronavirusIntent_Mask)) + stat_smooth() #heavy libs also high?

ggplot(surveys, aes(x=politicalParty, y=coronavirusIntent_Mask)) + stat_smooth() #saggy center

ggplot(surveys, aes(x=trumpApproval, y=coronavirusIntent_Mask)) + stat_smooth() #wow

ggplot(surveys, aes(x=politicalBelief, y=trumpApproval)) + stat_smooth() #sanity check
ggplot(surveys, aes(x=politicalParty, y=trumpApproval, color=ethnicity)) + stat_smooth() + geom_point(alpha=.1)



ggplot(surveys, aes(x=politicalParty, y=trumpApproval, color=belief_cluster)) + stat_smooth() + geom_point(alpha=.1)

ggplot(surveys, aes(x=coronavirusIntent_Mask, fill=belief_cluster)) + geom_density(alpha=.5)