package business.Order;

import model.Card;
import model.Country;
import model.Player;

/**
 * 
 * @author Kevin
 * @version build 2
 */
public class BombOrder implements Order{

	private Country targetCountry;
	private Player player;
	
	
	public BombOrder(Player p_player, Country p_targetCountry) {
		super();
		player = p_player;
		targetCountry = p_targetCountry;
		
	}
	

	
	@Override
	public void execute() {
		
		int beforeBomb = targetCountry.getArmies();
		
		int afterBomb = beforeBomb/2;
		
		targetCountry.setArmy(afterBomb);
		
		
	}

	//implemented removing bomb card after using it
	
	@Override
	public boolean valid() {
		
		boolean hasCard = false;
		
		for(Card card : player.getCardList()) {
			if(card.getCardType() == Card.CardType.BOMB) {
				hasCard = true;
			}
		}
		if(!hasCard) {
			return false;
		}
		
		if(targetCountry == null) {
			return false;
		}
		else if(!player.getCountriesHold().contains(targetCountry)) {
			for(Country country : player.getCountriesHold()) {
				if(country.getNeighbors().contains(targetCountry)) {
					
					for(Card card : player.getCardList()) {
						if(card.getCardType() == Card.CardType.BOMB) {
							player.getCardList().remove(card);
							break;
						}
					}
					
					return true;
				}
			}
		}
		
		return false;
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}