# GGP-Botter

GGP-Botter is a GGP Bot framework written in SWI-Prolog. It provides an interface for communication with GGP Server, as well as some helper functions (TODO) which will come in handy when creating your own bot.

<strong>WORK IN PROGRESS</strong>

## Installation

Simply clone this repository into your desired location:

```bash
git clone https://github.com/michalkurzeja/GGP-Botter.git <destination>
```

Then go to ```config``` directory and rename ```parameters.json.dist``` to ```parameters.json```.

## Running the Bot

To start up the bot you need to run ```serverApi:start```.

E.g. (working directory is ```<root>/src```):

```bash
swipl         # open prolog console
```
```bash
[serverApi].  # import module
```
```bash
start.        # run the server
```

That's it!

### Configuration

You may edit ```config/parameters.json``` file to change some options, e.g. ```port```.