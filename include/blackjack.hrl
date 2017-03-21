%% Card representation
%%
%% Suite have can take the following values
%%  - club
%%  - spade
%%  - heart
%%  - diamond
%%
%%  Value is a range between 1 and 13.
%%   - 1 is Ace
%%   - 11 is Jack
%%   - 12 is Queen
%%   - 13 is King
%%
-record(card, {value, suite}).
