package business.Order;

import model.Card;
import model.Country;
import model.Player;

/**
 * 
 * @author Kevin
 * @version build 2
 */
public class BlockadeOrder implements Order{

	private Country targetCountry;
	private Player player;
	
	public BlockadeOrder(Player p_player, Country p_targetCountry) {
		super();
		player = p_player;
		targetCountry = p_targetCountry;
	}
	
	
	@Override
	public void execute() {

		player.removeCountryHold(targetCountry);
		
		int doubleArmies = 3 * targetCountry.getArmies();
		
		targetCountry.setArmy(doubleArmies);
		
		targetCountry.setNeutral();
		
	}

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

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}