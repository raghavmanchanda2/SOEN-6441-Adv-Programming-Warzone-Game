import logger.GeneralException;
import model.Tournament.TournamentDetails;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class TournamentEngineTest extends TournamentEngine{

    TournamentEngine tournamentEngine ;
    TournamentDetails d_Details = new TournamentDetails();

    /**
     * Method to set up the basics for each test case
     *
     * @throws Exception if any exception occurs
     */
    @Before
    public void setUp() throws Exception {
        tournamentEngine = new TournamentEngineTest();
    }

    

    /**
     * Check possibilities of invalid options in a tournament command
     */
    @Test
    public void checkInvalidCommandOptions() {
        String l_TournamentCommand = "tournament -M Australia.map -P aggressive,random -G 6 -D 3";
        d_Details = this.parseCommand(l_TournamentCommand);
        assertNull(d_Details);
        l_TournamentCommand = "tournament -M Australia.map -P aggressive,random -G 2 -D -9";
        d_Details = this.parseCommand(l_TournamentCommand);
        assertNull(d_Details);
        l_TournamentCommand = "tournament -M Australia.map -P aggressive,random -G 2 -D 60";
        d_Details = this.parseCommand(l_TournamentCommand);
        assertNull(d_Details);
        l_TournamentCommand = "tournament -M Australia.map -P aggressive -G 2 -D 60";
        d_Details = this.parseCommand(l_TournamentCommand);
        assertNull(d_Details);
    }


    /*@Test
    public void checkValidResultOfTournament() throws GeneralException {
        String l_TournamentCommand = "tournament -M Australia.map -P aggressive,random -G 2 -D 3";
        this.parseCommand(l_TournamentCommand);
        this.startTournamentMode();
        assertEquals(2,this.d_Results.size());
    }*/
}
