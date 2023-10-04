/**
 * The `WarzoneEngineController` class is responsible for handling user input related to game features in the Warzone game.
 * It allows users to select various game features and returns the selected feature as an integer.
 * 
 * <p>
 * This class is used to gather user input for selecting game features.
 * </p>
 * 
 * @version 1.0
 */
package controller;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The `WarzoneEngineController` class is responsible for handling user input related to game features in the Warzone game.
 * It allows users to select various game features and returns the selected feature as an integer.
 * 
 * <p>
 * This class is used to gather user input for selecting game features.
 * </p>
 * 
 * @version 1.0
 */
public class WarzoneEngineController {
	
	private Scanner d_inputForFeatureSelection;
	
	/**
	 * Default constructor for the `WarzoneEngineController` class.
	 * Initializes the input scanner for feature selection.
	 */
	public WarzoneEngineController(){
		d_inputForFeatureSelection = new Scanner(System.in);
	}
	
	/**
	 * Gets the user's selected game feature as an integer.
	 * 
	 * @return The selected game feature as an integer. Returns 404 if the input is invalid.
	 */
	public int getGameFeatureInputs() {
		int l_selectedFeature = 404;
		try {
			l_selectedFeature = d_inputForFeatureSelection.nextInt();
			
		
		}catch(InputMismatchException p_inputMismatchException){
			// Throws a global exception
			System.out.println("Please provide proper input");
			return 404;
		}
		
		if (l_selectedFeature > 0  && l_selectedFeature <= 4)
			return l_selectedFeature;
		else
			return 404;
	}
}
