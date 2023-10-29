package business;

import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;
import business.Order.*;

public class MainPlayPhaseBusinessCommands {
	
	private MapModel mapModel;
	
	public MainPlayPhaseBusinessCommands() {
		mapModel = MapModel.getInstance();
	}
	
	public ResponseWrapper doReinforcements() {
		// do reinforcements
		
		
		return null;
		
	}
	
	public ResponseWrapper deploy(Player currentPlayer, String country , int numerOfarmies) {
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
	

	public ResponseWrapper advance(Player currentPlayer, String countryNameFrom, String countryNameTo, int numerOfarmies) {
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
	
	
	public ResponseWrapper bomb(Player currentPlayer, String targetCountryName) {
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
	
}
