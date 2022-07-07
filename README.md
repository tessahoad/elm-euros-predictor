# World Cup Prognostication #

This week we're doing a fun game from Programming Praxis:

[World Cup Prognostication](https://programmingpraxis.com/2010/06/29/world-cup-prognostication/)
> On Saturday morning, inspired by Andrew Moylan’s article on Wolfram’s Blog, I sat down to work out a simulation of the knockout stage of the World Cup competition. I used the bracket shown at right, and found elo ratings of the sixteen teams, as of that morning, at Wikipedia:
>
> 1 BRA Brazil        2082 
> 2 ESP Spain         2061
> 3 NED Netherlands   2045
> 4 ARG Argentina     1966
> 5 ENG England       1945
> 6 GER Germany       1930 
> 7 URU Uruguay       1890 
> 8 CHI Chile         1883 
> 9 POR Portugal      1874 
> 10 MEX Mexico        1873 
> 15 USA United States 1785 
> 19 PAR Paraguay      1771 
> 25 KOR Korea         1746 
> 26 JPN Japan         1744 
> 32 GHA Ghana         1711 
> 45 SVK Slovakia      1654 
>
> The table shows that there are forty-four national teams with ratings higher than Slovakia’s rating of 1654; they are lucky to be in the tournament. 
> 
> The likelihood that a team will win its match can be computed from the elo rankings of the team and its opponent according to the formula \frac{1}{1 + 10^{(elo_{them} - elo_{us})/400}}. Thus, the United States had a 60.5% expectation of winning its match against Ghana this afternoon, and Ghana had a 39.5% expectation of defeating the United States. Harrumph!
>
>Every time a match is played, the elo rating of a team changes. The amount of the change is based on the actual result as compared to the expected result. If a team wins when they have a high expectation of winning, their elo rating goes up by a small amount, since they were expected to win. However, if a team wins when they have a low expectation of winning, their elo rating goes up by a large amount. The formula is elo_{new} = elo_{old} + KG(W-W_e), where K is a weighting for the importance of the game (K is 60 for the World Cup), G is a parameter based on the goal differential (we’ll assume that all games are won by a single goal, so G = 1), W is 1 for a win and 0 for a loss, and We is the winning expectation calculated by the formula given above.
>
>Your task is to use the data and formulas described above to simulate the knockout stage of the World Cup a million times and report the number of times each nation wins. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.

The ELO rankings of the various teams is available on [Wikipedia](https://en.wikipedia.org/wiki/World_Football_Elo_Ratings),
and the brackets are available in a several places, including [Wikipedia](https://en.wikipedia.org/wiki/2018_FIFA_World_Cup#Schedule).

To save you a little time looking things up, you can calculate the probability of team A beating team B using the
following formula:

    P(A) = 1/(1+10^m) where m is the rating difference (rating(B)-rating(A)) divided by 400

For fun, let's try doing this for the Women's Euro 2022 tournament, which is running now:

- Brackets are on [Wikipedia](https://en.wikipedia.org/wiki/UEFA_Women%27s_Euro_2022)
- ELO ratings are on [Everybody Soccer](https://everybodysoccer.com/even-the-goalkeepers-like-to/2016/8/20/womens-international-football-elo-ratings)

To save you a little time, here's what they currently are:

```
    England                     1692
    Germany                     1775
    Netherlands                 1726
    Denmark                     1503
    Norway                      1537
    Sweden                      1801
    France                      1832
    Belgium                     1374
    Iceland                     1476
    Spain                       1668
    Finland                     1210
    Austria                     1380
    Italy                       1582
    Switzerland                 1327
    Northern Ireland             980
    Portugal                    1248
```
