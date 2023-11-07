package business.Order;

import model.Card;
import model.Player;

/**
 * Class that defines diplomacy functionalities
 * 
 * @author spankeydamankey
 * @version build 2
 */
public class DiplomacyOrder implements Order{
	
	Player currentPlayer;
	Player peaceWith;
	
	/**
	 * parameterized constructor that to build a diplomacy order
	 * @param currentPlayer - player that will execute diplomacy
	 * @param peaceWith - other player that will make peace with
	 */
	public DiplomacyOrder(Player currentPlayer, Player peaceWith) {
		this.currentPlayer = currentPlayer;
		this.peaceWith = peaceWith;
	}

	/**
	 * set peace between both players
	 */
	@Override
	public void execute() {
		currentPlayer.setPeaceWith(peaceWith);
		peaceWith.setPeaceWith(currentPlayer);
	}

	/**
	 * 1. check if player that initiates diplomacy has a diplomacy card
	 * 2. check if player that receives diplomacy order is valid
	 * 3. If above are true, remove diplomacy card from the player
	 */
	@Override
	public boolean valid() {
		
		boolean hasCard = false;
		
		for(Card card : currentPlayer.getCardList()) {
			if(card.getCardType() == Card.CardType.DIPLOMACY) {
				hasCard = true;
			}
		}
		if(!hasCard) {
			return false;
		}
		
		if(peaceWith != null) {
			
			for(Card card : currentPlayer.getCardList()) {
				if(card.getCardType() == Card.CardType.DIPLOMACY) {
					currentPlayer.getCardList().remove(card);
					break;
				}
			}
			
			return true;
		}
		
		return false;
	}

	/**
	 * Print execution of diplomacy order
	 */
	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}
