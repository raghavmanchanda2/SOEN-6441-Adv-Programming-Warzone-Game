package business;

import model.Country;
import model.Player;
import model.ResponseWrapper;
import business.Order.*;

public class MainPlayPhaseBusinessCommands {
	
	
	
	
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
			return new ResponseWrapper(204,"from coutry doesn't belongs to you or targetted country is not your neighbour");
		}
		
	}
	
	
}
