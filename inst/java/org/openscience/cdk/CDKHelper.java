package org.openscience.cdk;

import org.openscience.cdk.interfaces.*;
import org.openscience.cdk.exception.*;
import toxTree.query.MolFlags;
import toxTree.core.*;
import toxTree.tree.cramer.*;

/**
 * Helper class to work around rJava's type-matching limitations when calling
 * CDK 2.9's package-private setProperty(Object, Object) from R.
 *
 * CDK 2.9's AtomContainer2 overrides IAtomContainer.setProperty(String, Object)
 * with setProperty(Object, Object), but the class and the overriding method are
 * both package-private (not public). rJava's .jcall() dispatches on static
 * declared types, not runtime types, so it cannot reach the (Object, Object)
 * overload from R. This class lives in the same package as AtomContainer2 and
 * calls setProperty directly on the IAtomContainer reference that Toxtree's
 * rules expect.
 *
 * All public methods catch checked exceptions and return null on failure,
 * allowing the R caller to handle errors gracefully without declaring throws.
 */
public class CDKHelper {

    private static volatile org.openscience.cdk.inchi.InChIGeneratorFactory _factory;
    private static volatile org.openscience.cdk.smiles.SmilesParser _parser;

    /**
     * Set the MolFlags property on a molecule so that Toxtree's rule checks
     * (e.g. RuleAnySubstituents) do not throw "Structure should be preprocessed!".
     *
     * @param mol IAtomContainer, typically an AtomContainer2 from CDK 2.9
     * @return true on success, false on failure
     */
    public static boolean setMolFlags(IAtomContainer mol) {
        try {
            String key = MolFlags.MOLFLAGS;
            MolFlags mf = new MolFlags();
            mol.setProperty(key, mf);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Return the cached InChIGeneratorFactory (singleton pattern).
     * Call this after all JARs are loaded so the JNA-backed factory from
     * rcdklibs is registered before Toxtree's JNI-backed factory is attempted.
     *
     * @return the singleton InChIGeneratorFactory, or null on failure
     */
    public static org.openscience.cdk.inchi.InChIGeneratorFactory getInChIFactory() {
        try {
            if (_factory == null) {
                synchronized (CDKHelper.class) {
                    if (_factory == null) {
                        _factory = org.openscience.cdk.inchi.InChIGeneratorFactory.getInstance();
                    }
                }
            }
            return _factory;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Get InChI string for a molecule via the JNA-backed factory cached by
     * getInChIFactory(). The returned string has the "InChI=1S/" prefix.
     *
     * @param mol IAtomContainer, must already have MolFlags set
     * @return InChI string, or null if generation failed
     */
    public static String getInChI(IAtomContainer mol) {
        try {
            org.openscience.cdk.inchi.InChIGeneratorFactory f = getInChIFactory();
            if (f == null) return null;
            org.openscience.cdk.inchi.InChIGenerator gen = f.getInChIGenerator(mol);
            return gen.getInchi();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Convert an InChI string to its InChIKey (standardised hash).
     *
     * @param inchi InChI string (with "InChI=1S/" prefix)
     * @return 27-character InChIKey, or null on failure
     */
    public static String inchiToInchiKey(String inchi) {
        try {
            if (inchi == null) return null;
            io.github.dan2097.jnainchi.InchiKeyOutput out =
                io.github.dan2097.jnainchi.JnaInchi.inchiToInchiKey(inchi);
            return out.getInchiKey();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Parse a SMILES string into an IAtomContainer using a shared parser.
     * The parser is created lazily and is thread-safe for read-only use.
     *
     * @param smiles SMILES string
     * @param builder CDK builder (use DefaultChemObjectBuilder.getInstance())
     * @return IAtomContainer, or null if parsing failed
     */
    public static IAtomContainer parseSmiles(String smiles,
            org.openscience.cdk.interfaces.IChemObjectBuilder builder) {
        try {
            if (_parser == null) {
                synchronized (CDKHelper.class) {
                    if (_parser == null) {
                        _parser = new org.openscience.cdk.smiles.SmilesParser(builder);
                    }
                }
            }
            return _parser.parseSmiles(smiles);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Run the Toxtree CramerRules classification on a molecule that already has
     * MolFlags set (via setMolFlags). This method encapsulates the full
     * classification workflow:
     *   1. initialise(builder)
     *   2. createDecisionResult()
     *   3. verifyRules(mol, result)
     *
     * @param mol    IAtomContainer with MolFlags property set
     * @param builder IChemObjectBuilder (for the CramerRules tree to initialise with)
     * @return IDecisionResult containing the category and rule-level results,
     *         or null on failure
     */
    public static toxTree.core.IDecisionResult runCramerRules(
            IAtomContainer mol,
            org.openscience.cdk.interfaces.IChemObjectBuilder builder) {
        try {
            CramerRules cramer = new CramerRules();
            cramer.initialise(builder);
            IDecisionResult result = cramer.createDecisionResult();
            cramer.verifyRules(mol, result);
            return result;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Classify a molecule via CramerRules in one shot, returning whether it
     * passed the full decision tree (i.e. was not excluded by any rule).
     *
     * @param mol    IAtomContainer with MolFlags set
     * @param builder IChemObjectBuilder
     * @return true if classified (getCategory() != null), false if null input
     */
    public static boolean classifyMol(
            IAtomContainer mol,
            org.openscience.cdk.interfaces.IChemObjectBuilder builder) {
        try {
            if (mol == null || builder == null) return false;
            CramerRules cramer = new CramerRules();
            cramer.initialise(builder);
            IDecisionResult result = cramer.createDecisionResult();
            cramer.verifyRules(mol, result);
            return result.getCategory() != null;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Extract the Cramer category name from an IDecisionResult.
     * Category names are typically "Low (Class I)", "Moderate (Class II)",
     * "High (Class III)" for the classic Cramer scheme.
     *
     * @param result IDecisionResult from runCramerRules()
     * @return category name string, or null on failure
     */
    public static String getCramerCategory(toxTree.core.IDecisionResult result) {
        try {
            if (result == null) return null;
            IDecisionCategory cat = result.getCategory();
            return cat != null ? cat.getName() : null;
        } catch (Exception e) {
            return null;
        }
    }

}
