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
	
	/**
	 * Method that converts input string commands into objects to be used for deploy execution
	 * @param p_currentPlayer - Current player object that is inputing string command
	 * @param p_country - Country where armies will be deployed in
	 * @param p_numerOfarmies - Number of armies that will be deployed
	 */
	@Override
	public ResponseWrapper deploy(Player p_currentPlayer, String p_country , int p_numerOfarmies) throws GeneralException{
		Country targettedCountry = null;
		for(Country countryInList : p_currentPlayer.getCountriesHold()) {
			if(countryInList.getCountryId().equals(p_country)) {
				targettedCountry = countryInList;
			}
		}
		
		
		Order deploy = new DeployOrder(targettedCountry,p_numerOfarmies,p_currentPlayer);
		if(deploy.valid()) {
			p_currentPlayer.addOrder(deploy);
			return new ResponseWrapper(200, " Deploy order added in queue");
		}else {
			return new ResponseWrapper(204,"Targeted country doesn't belong to you");
		}
		
		
	}
	
	/**
	 * Method that converts input string commands into objects to be used for advance execution
	 * @param p_currentPlayer - Current player object that is inputing string command
	 * @param p_countryNameFrom - Source country where armies are moving from
	 * @param p_countryNameTo - Destination country where armies are moving to
	 * @param p_numerOfarmies - Number of armies being displaced or attacking
	 */
	@Override
	public ResponseWrapper advance(Player p_currentPlayer, String p_countryNameFrom, String p_countryNameTo, int p_numerOfarmies) throws GeneralException{
		Country countryFrom = null;
		Country countryTo = null;
		for(Country countryInList : p_currentPlayer.getCountriesHold()) {
			if(countryInList.getCountryId().equals(p_countryNameFrom)) {
				countryFrom = countryInList;
				for(Country countryInNeighbours : countryInList.getNeighbors()) {
					if(countryInNeighbours.getCountryId().equals(p_countryNameTo)) {
						countryTo = countryInNeighbours;
					}
				}
			}
		}
		
		Order advance = new AdvanceOrder(countryFrom, countryTo, p_numerOfarmies, p_currentPlayer);
		if(advance.valid()) {
			p_currentPlayer.addOrder(advance);
			return new ResponseWrapper(200, " Advance order added in queue");
		}else {
			return new ResponseWrapper(204,"from country doesn't belong to you or targeted country is not your neighbour");
		}
		
	}
	
	/**
	 * Method that converts input string commands into objects to be used for bomb execution
	 * @param p_currentPlayer - Current player object that is inputting string command
	 * @param p_targetCountryName - Name of country that will be bombed
	 * @return alert message that bomb is successful or unsuccessful
	 */
	@Override
	public ResponseWrapper bomb(Player p_currentPlayer, String p_targetCountryName) throws GeneralException{
		Country targetCountry = null;
		
		for(Country country : mapModel.getCountries()) {
			if(country.getCountryId().equals(p_targetCountryName)) {
				targetCountry = country;
				break;
			}
		}
		
		Order bomb = new BombOrder(p_currentPlayer, targetCountry);
		if(bomb.valid()) {
			p_currentPlayer.addOrder(bomb);
			return new ResponseWrapper(200, " Bomb order added in queue");
		}else {
			return new ResponseWrapper(204,"One of the following occured: \n"
										+ "1. You do not possess a bomb card\n"
										+ "2. You are targetting a country that belongs to you\n"
										+ "3. You are targetting a country that is not adjacent to one of your countries\n"
										+ "4. That country does not exist in the map\n");
		}
	}
	
	/**
	 * Method that converts input string commands into objects to be used for blockade execution
	 * @param p_currentPlayer - Current player object that is inputing string command
	 * @param p_targetCountryName - Name of country in which a blockade is performed on
	 * @return alert message that blockade is successful or unsuccessful
	 */
	@Override
	public ResponseWrapper blockade(Player p_currentPlayer, String p_targetCountryName) throws GeneralException {
		Country targetCountry = null;
		
		for(Country country : p_currentPlayer.getCountriesHold()) {
			if(country.getCountryId().equals(p_targetCountryName)) {
				targetCountry = country;
				break;
			}
		}
		
		Order blockade = new BlockadeOrder(p_currentPlayer, targetCountry);
		if(blockade.valid()) {
			p_currentPlayer.addOrder(blockade);
			return new ResponseWrapper(200, " Blockade order added in queue");
		}else {
			return new ResponseWrapper(204, "Can only perform blockade on your own country\n");
		}
	}
	
	/**
	 * Method that converts input string commands into objects to be used for airlift execution
	 * @param p_currentPlayer - Current player object that is inputing string command
	 * @param p_countryNameFrom - Source country that will airlift the armies to a destination
	 * @param p_countryNameTo - Destination Country that will receive armies from airlift
	 * @param p_numArmies - Number of armies being displaced by airlift
	 * @return alert message that airlift is successful or unsuccessful
	 */
	@Override
	public ResponseWrapper airlift(Player p_currentPlayer, String p_countryNameFrom, String p_countryNameTo, int p_numArmies)  throws GeneralException {
		Country countryFrom = null;
		Country countryTo = null;
		
		for(Country country : p_currentPlayer.getCountriesHold()) {
			if(country.getCountryId().equals(p_countryNameFrom)) {
				countryFrom = country;
			}
			
			if(country.getCountryId().equals(p_countryNameTo)) {
				countryTo = country;
			}
		}
		
		Order airlift = new AirliftOrder(p_currentPlayer, countryFrom, countryTo, p_numArmies);
		if(airlift.valid()) {
			p_currentPlayer.addOrder(airlift);
			return new ResponseWrapper(200, " Airlift order added in queue");
		}else {
			return new ResponseWrapper(204, "One of the following occured: \n"
										+ "1. You do not possess an airlift card\n"
										+ "2. Country you are airlifting from does not belongs to you\n"
										+ "3. Country you are airlifting to does not belongs to you\n"
										+ "4. That country does not exist in the map\n");
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
