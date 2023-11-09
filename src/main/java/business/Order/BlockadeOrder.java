package business.Order;

import model.Card;
import model.Country;
import model.Player;

/**
 * Class that defines blockade functionalities
 * 
 * @author Kevin
 * @author ishaanbajaj
 * @version build 2
 */
public class BlockadeOrder implements Order{


	/**
	 * Object of class Country - the country to be targeted.
	 */
	private Country targetCountry;
	/**
	 * Object of class player - to get current turn player
	 */
	private Player player;
	
	/**
	 * Parameterized constructor that to build a blockade order
	 * @param p_player - player that wants to execute blockade order
	 * @param p_targetCountry - blockade will occur in this country
	 */
	public BlockadeOrder(Player p_player, Country p_targetCountry) {
		super();
		player = p_player;
		targetCountry = p_targetCountry;
	}
	
	/**
	 * Triples the army in the target country and sets it to neutral
	 */
	@Override
	public void execute() {

		player.removeCountryHold(targetCountry);
		
		int doubleArmies = 3 * targetCountry.getArmies();
		
		targetCountry.setArmy(doubleArmies);
		
		targetCountry.setNeutral();
		
	}

	/**
	 * 1. check if player has a blockade card
	 * 2. check if the target country belongs to the player
	 * 3. If the above are true, remove blockade card from the player and return true
	 */
	@Override
	public boolean valid() {

		boolean hasCard = false;
		
		for(Card card : player.getCardList()) {
			if(card.getCardType() == Card.CardType.BLOCKADE) {
				hasCard = true;
			}
		}
		if(!hasCard) {
			return false;
		}
		
		if(targetCountry == null) {
			return false;
		}
		else if(player.getCountriesHold().contains(targetCountry)) {
			
			for(Card card : player.getCardList()) {
				if(card.getCardType() == Card.CardType.BLOCKADE) {
					player.getCardList().remove(card);
					break;
				}
			}
			
			return true;
		}
		
		return false;
	}

	/**
	 * Print execution of blockade order
	 */
	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}