# GGP-Botter

GGP-Botter is a GGP Bot framework written in SWI-Prolog. It provides an interface for communication with GGP Server, as well as some helper functions (TODO) which will come in handy when creating your own bot.

## Installation

Simply clone this repository into your desired location:

```bash
git clone https://github.com/michalkurzeja/GGP-Botter.git <destination>
```

Then go to ```config``` directory and rename ```parameters.json.dist``` to ```parameters.json```.

## Running the Bot

To start up the bot you need to run ```bot.pl``` script with 2 arguments: ```port``` and ```bot_file```.

```port``` - integer
```bot_file``` - file with bot code, located in directory specified in ```parameters.json``` file.

Example:

```bash
./bot.pl 9147 random.pl
```

That's it! Bot is ready do play!

### Bot interface

Any bot must implement 5 predicates to work:

```start``` - here should be performed any actions that are required before the game starts. At this point we have all the rules loaded in ```db``` module. All necessary information can be accessed via ```game``` module predicates (listed in the next section).

```play(Played)``` - here the bot should compute the next move and return it as ```Played```. The game state has already been updated by now.

```stop``` - called when the game has been ended under normal conditions. Bot should perform cleanup here.

```abort``` - called when the game has been terminated abnormally. Cleanup here as well.

### Game API

The bot has access to the following methods that help with rules processing and game handling, via ```game``` module:

  * findGameId(-GameId).
  * findMyRole(-Role).
  * findStartClock(-Clock).
  * findPlayClock(-Clock).
  * findRoles(-Roles).
  * findPropositions(-Propositions).
  * findActions(+Role, -Actions).
  * findInitialState(-State).
  * findCurrentState(-State).
  * findFirstLegal(+Role, +State, -Action).
  * findRandomLegal(+Role, +State, -Action).
  * findAllLegal(+Role, +State, -Actions).
  * findNext(+Roles, +Moves, +State, -Next).
  * findReward(+Role, +State, -Reward).
  * isTerminal(+State).