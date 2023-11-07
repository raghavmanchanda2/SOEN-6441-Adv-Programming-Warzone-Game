package business.Order;

import model.Player;
import model.Card;
import model.Card.CardType;
import model.Country;

/**
 * 
 * Airlift that allows a single transfer of armies from any of your territories.
 * Must wait next turn to attack.
 * Transfer occurs before attack takes place.
 * 
 * @author Kevin
 * @version build 2
 */
public class AirliftOrder implements Order{
	
	private Country originCountry;
	private Country destinationCountry;
	private int armies_to_move;
	private Player player;
	
	/**
	 * Parameterized constructor to build an airlift order
	 * @param player - the one that wants to execute an airlift order
	 * @param countryFrom - source country
	 * @param countryTo - destination country
	 * @param armies_to_move - amount of armies to airlift
	 */
	public AirliftOrder(Player player, Country countryFrom, Country countryTo, int armies_to_move) {
		this.player = player;
		originCountry = countryFrom;
		destinationCountry = countryTo;
		this.armies_to_move = armies_to_move;
	}
	
	/**
	 * Execute airlift by moving the armies from source country to target country
	 */
	@Override
	public void execute() {
		
		int originCountryArmyBefore = originCountry.getArmies();
		int originCountryArmyAfter = originCountryArmyBefore - armies_to_move;
		originCountry.setArmy(originCountryArmyAfter);
		
		int destinationCountryArmyBefore = destinationCountry.getArmies();
		int destinationCountryArmyAfter = destinationCountryArmyBefore + armies_to_move;
		destinationCountry.setArmy(destinationCountryArmyAfter);
		
		
	}
	
	/**
	 * 1. Player requires airlift card in order for execution to be valid
	 * 2. source and destination countries must belong to the player and source and destination cannot be the same
	 * 3. number of armies to move must be valid
	 */
	@Override
	public boolean valid() {
		
		boolean hasCard = false;
		
		for(Card card : player.getCardList()) {
			if(card.getCardType() == Card.CardType.AIRLIFT) {
				hasCard = true;
			}
		}
		if(!hasCard) {
			return false;
		}
		
		if(originCountry == null || destinationCountry == null
		|| !player.getCountriesHold().contains(originCountry) 
		|| !player.getCountriesHold().contains(destinationCountry)
		|| originCountry == destinationCountry) {
			return false;
		}
		else 
		if(armies_to_move < originCountry.getArmies() && armies_to_move > 0) {
			
			for(Card card : player.getCardList()) {
				if(card.getCardType() == Card.CardType.AIRLIFT) {
					player.getCardList().remove(card);
					break;
				}
			}
			
			return true;
		}
		
		return false;
	}
	
	/**
	 * Print execution of airlift order
	 */
	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}