package business;

import model.Continent;
import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;
import business.Order.*;
import logger.GeneralException;

public class MainPlayPhaseBusinessCommands extends Phase {
	
	private MapModel mapModel;
	
	public MainPlayPhaseBusinessCommands() {
		mapModel = MapModel.getInstance();
	}
	
	@Override
	public ResponseWrapper doReinforcements() throws GeneralException{
		return printInvalidCommandInState();
		
	}
	
	@Override
	public ResponseWrapper deploy(Player currentPlayer, String country , int numerOfarmies) throws GeneralException{
		Country targettedCountry = null;
		for(Country countryInList : currentPlayer.getCountriesHold()) {
			if(countryInList.getCountryId().equals(country)) {
				targettedCountry = countryInList;
			}
		}
		
		
		Order deploy = new DeployOrder(targettedCountry,numerOfarmies,currentPlayer);
		if(deploy.valid()) {
			currentPlayer.addOrder(deploy);
			return new ResponseWrapper(200, " Deploy order added in queue");
		}else {
			return new ResponseWrapper(204,"Targetted coutry doesn't belongs to you");
		}
		
		
	}
	
	@Override
	public ResponseWrapper advance(Player currentPlayer, String countryNameFrom, String countryNameTo, int numerOfarmies) throws GeneralException{
		Country countryFrom = null;
		Country countryTo = null;
		for(Country countryInList : currentPlayer.getCountriesHold()) {
			if(countryInList.getCountryId().equals(countryNameFrom)) {
				countryFrom = countryInList;
				for(Country countryInNeighbours : countryInList.getNeighbors()) {
					if(countryInNeighbours.getCountryId().equals(countryNameTo)) {
						countryTo = countryInNeighbours;
					}
				}
			}
		}
		
		Order advance = new AdvanceOrder(countryFrom,countryTo,numerOfarmies,currentPlayer);
		if(advance.valid()) {
			currentPlayer.addOrder(advance);
			return new ResponseWrapper(200, " Advance order added in queue");
		}else {
			return new ResponseWrapper(204,"from country doesn't belongs to you or targetted country is not your neighbour");
		}
		
	}
	
	@Override
	public ResponseWrapper bomb(Player currentPlayer, String targetCountryName) throws GeneralException{
		Country targetCountry = null;
		
		for(Country country : mapModel.getCountries()) {
			if(country.getCountryId().equals(targetCountryName)) {
				targetCountry = country;
			}
		}
		
		Order bomb = new BombOrder(currentPlayer, targetCountry);
		if(bomb.valid()) {
			currentPlayer.addOrder(bomb);
			return new ResponseWrapper(200, " Bomb order added in queue");
		}else {
			return new ResponseWrapper(204,"One of the following occured: \n"
										+ "1. You do not possess a bomb card\n"
										+ "2. You are targetting a country that belongs to you\n"
										+ "3. You are targetting a country that is not adjacent to one of your countries\n"
										+ "4. That country does not exist in the map\n");
		}
	}
	
	@Override
	public ResponseWrapper blockade(Player currentPlayer, String targetCountryName) throws GeneralException {
		Country targetCountry = null;
		
		for(Country country : currentPlayer.getCountriesHold()) {
			if(country.getCountryId().equals(targetCountryName)) {
				targetCountry = country;
			}
		}
		
		Order blockade = new BlockadeOrder(currentPlayer, targetCountry);
		if(blockade.valid()) {
			currentPlayer.addOrder(blockade);
			return new ResponseWrapper(200, " Blockade order added in queue");
		}else {
			return new ResponseWrapper(204, "Can only perform blockade on your own country\n");
		}
	}

	@Override
	public ResponseWrapper showMap() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper assignCountries() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editContinent(Continent p_continent, String command) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper addPlayerInGame(String playerName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper removeplayerFromGame(String playerName) throws GeneralException {
		printInvalidCommandInState();
		return new ResponseWrapper(404, "Incorrect Command");
	}

	@Override
	public ResponseWrapper afterCommitReinforcement() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editNeighbour(Country p_mainCountry, Country p_neighbourCountry, String command) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editCountry(Country p_country, String command) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper validateMap() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper loadMap(String map) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper saveMap(String p_mapFileName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editOrCreateMap(String p_mapFileName) throws GeneralException {
		return printInvalidCommandInState();
	}
	
}
