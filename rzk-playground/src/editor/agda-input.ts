import { Extension } from '@codemirror/state';
import { keymap } from '@codemirror/view';

// Unicode input mappings based on Agda's input method
// Maps LaTeX commands to Unicode characters
const unicodeMap: Record<string, string> = {
    // Greek letters
    'alpha': 'α',
    'beta': 'β',
    'gamma': 'γ',
    'delta': 'δ',
    'epsilon': 'ε',
    'zeta': 'ζ',
    'eta': 'η',
    'theta': 'θ',
    'iota': 'ι',
    'kappa': 'κ',
    'lambda': 'λ',
    'mu': 'μ',
    'nu': 'ν',
    'xi': 'ξ',
    'pi': 'π',
    'rho': 'ρ',
    'sigma': 'σ',
    'tau': 'τ',
    'upsilon': 'υ',
    'phi': 'φ',
    'chi': 'χ',
    'psi': 'ψ',
    'omega': 'ω',
    'Alpha': 'Α',
    'Beta': 'Β',
    'Gamma': 'Γ',
    'Delta': 'Δ',
    'Epsilon': 'Ε',
    'Zeta': 'Ζ',
    'Eta': 'Η',
    'Theta': 'Θ',
    'Iota': 'Ι',
    'Kappa': 'Κ',
    'Lambda': 'Λ',
    'Mu': 'Μ',
    'Nu': 'Ν',
    'Xi': 'Ξ',
    'Pi': 'Π',
    'Rho': 'Ρ',
    'Sigma': 'Σ',
    'Tau': 'Τ',
    'Upsilon': 'Υ',
    'Phi': 'Φ',
    'Chi': 'Χ',
    'Psi': 'Ψ',
    'Omega': 'Ω',
    
    // Mathematical symbols
    'forall': '∀',
    'exists': '∃',
    'nexists': '∄',
    'in': '∈',
    'notin': '∉',
    'ni': '∋',
    'notni': '∌',
    'subset': '⊂',
    'supset': '⊃',
    'subseteq': '⊆',
    'supseteq': '⊇',
    'cup': '∪',
    'cap': '∩',
    'sqcup': '⊔',
    'sqcap': '⊓',
    'vee': '∨',
    'wedge': '∧',
    'vdash': '⊢',
    'dashv': '⊣',
    'models': '⊨',
    'top': '⊤',
    'bot': '⊥',
    'perp': '⊥',
    'nvdash': '⊬',
    'vDash': '⊨',
    'nvDash': '⊭',
    'Vdash': '⊩',
    'nVdash': '⊮',
    'VDash': '⊪',
    'nVDash': '⊫',
    
    // Arrows
    'to': '→',
    'gets': '←',
    'leftarrow': '←',
    'rightarrow': '→',
    'Leftarrow': '⇐',
    'Rightarrow': '⇒',
    'leftrightarrow': '↔',
    'Leftrightarrow': '⇔',
    'mapsto': '↦',
    'hookleftarrow': '↩',
    'hookrightarrow': '↪',
    'leftharpoonup': '↼',
    'leftharpoondown': '↽',
    'rightharpoonup': '⇀',
    'rightharpoondown': '⇁',
    'leadsto': '⇝',
    'uparrow': '↑',
    'downarrow': '↓',
    'Uparrow': '⇑',
    'Downarrow': '⇓',
    'updownarrow': '↕',
    'Updownarrow': '⇕',
    
    // Relations
    'equiv': '≡',
    'nequiv': '≢',
    'approx': '≈',
    'sim': '∼',
    'simeq': '≃',
    'cong': '≅',
    'asymp': '≍',
    'propto': '∝',
    'neq': '≠',
    'ne': '≠',
    'leq': '≤',
    'le': '≤',
    'geq': '≥',
    'ge': '≥',
    'prec': '≺',
    'succ': '≻',
    'preceq': '≼',
    'succeq': '≽',
    'll': '≪',
    'gg': '≫',
    'sqsubset': '⊏',
    'sqsupset': '⊐',
    'sqsubseteq': '⊑',
    'sqsupseteq': '⊒',
    
    // Operators
    'times': '×',
    'div': '÷',
    'cdot': '⋅',
    'circ': '∘',
    'bullet': '•',
    'star': '⋆',
    'ast': '∗',
    'oplus': '⊕',
    'ominus': '⊖',
    'otimes': '⊗',
    'oslash': '⊘',
    'odot': '⊙',
    'pm': '±',
    'mp': '∓',
    'sum': '∑',
    'prod': '∏',
    'coprod': '∐',
    'int': '∫',
    'oint': '∮',
    'bigcup': '⋃',
    'bigcap': '⋂',
    'bigsqcup': '⨆',
    'bigsqcap': '⨅',
    'bigvee': '⋁',
    'bigwedge': '⋀',
    'bigoplus': '⨁',
    'bigotimes': '⨂',
    'bigodot': '⨀',
    
    // Logic
    'land': '∧',
    'lor': '∨',
    'lnot': '¬',
    'neg': '¬',
    'implies': '⇒',
    'iff': '⇔',
    
    // Set theory
    'setminus': '∖',
    'uplus': '⊎',
    
    // Type theory / Category theory
    'hom': '→',
    'comp': '∘',
    'o': '∘',
    
    // Miscellaneous
    'infty': '∞',
    'partial': '∂',
    'nabla': '∇',
    'ell': 'ℓ',
    'hbar': 'ℏ',
    'imath': 'ı',
    'jmath': 'ȷ',
    'prime': '′',
    'surd': '√',
    'triangle': '△',
    'Box': '□',
    'Diamond': '◇',
    'clubsuit': '♣',
    'diamondsuit': '♦',
    'heartsuit': '♥',
    'spadesuit': '♠',
    'flat': '♭',
    'natural': '♮',
    'sharp': '♯',
    'angle': '∠',
    'measuredangle': '∡',
    'sphericalangle': '∢',
    'parallel': '∥',
    'nparallel': '∦',
    'smile': '⌣',
    'frown': '⌢',
    'wr': '≀',
    'Join': '⋈',
    'bowtie': '⋈',
    'ltimes': '⋉',
    'rtimes': '⋊',
    'leftthreetimes': '⋋',
    'rightthreetimes': '⋌',
    'backsimeq': '⋍',
    'curlyvee': '⋎',
    'curlywedge': '⋏',
    'Subset': '⋐',
    'Supset': '⋑',
    'Cap': '⋒',
    'Cup': '⋓',
    'pitchfork': '⋔',
    'equalparallel': '⋕',
    'lessdot': '⋖',
    'gtrdot': '⋗',
    'lll': '⋘',
    'ggg': '⋙',
    'lesseqgtr': '⋚',
    'gtreqless': '⋛',
    'eqless': '⋜',
    'eqgtr': '⋝',
    'curlyeqprec': '⋞',
    'curlyeqsucc': '⋟',
    'npreccurlyeq': '⋠',
    'nsucccurlyeq': '⋡',
    'nsqsubseteq': '⋢',
    'nsqsupseteq': '⋣',
    'sqsubsetneq': '⋤',
    'sqsupsetneq': '⋥',
    'lnsim': '⋦',
    'gnsim': '⋧',
    'precnsim': '⋨',
    'succnsim': '⋩',
    'ntriangleleft': '⋪',
    'ntriangleright': '⋫',
    'ntrianglelefteq': '⋬',
    'ntrianglerighteq': '⋭',
    'vdots': '⋮',
    'cdots': '⋯',
    'adots': '⋰',
    'ddots': '⋱',
    'barwedge': '⊼',
    'veebar': '⊽',
    'doublebarwedge': '⊾',
    'lnot-triangle': '⊿',
    'leftrightsquigarrow': '↭',
    'nleftarrow': '↚',
    'nrightarrow': '↛',
    'nLeftarrow': '⇍',
    'nRightarrow': '⇏',
    'nleftrightarrow': '↮',
    'nLeftrightarrow': '⇎',
    'dashleftarrow': '⇠',
    'dashrightarrow': '⇢',
    'leftleftarrows': '⇇',
    'leftrightarrows': '⇆',
    'rightrightarrows': '⇉',
    'rightleftarrows': '⇄',
    'leftarrowtail': '↢',
    'rightarrowtail': '↣',
    'twoheadleftarrow': '↞',
    'twoheadrightarrow': '↠',
    'leftrightharpoons': '⇋',
    'rightleftharpoons': '⇌',
    'nwarrow': '↖',
    'nearrow': '↗',
    'searrow': '↘',
    'swarrow': '↙',
    'Lleftarrow': '⇚',
    'Rrightarrow': '⇛',
    'longleftarrow': '⟵',
    'longrightarrow': '⟶',
    'longleftrightarrow': '⟷',
    'Longleftarrow': '⟸',
    'Longrightarrow': '⟹',
    'Longleftrightarrow': '⟺',
    'longmapsto': '⟼',
    'looparrowleft': '↫',
    'looparrowright': '↬',
    'Lsh': '↰',
    'Rsh': '↱',
    'dlsh': '↲',
    'drsh': '↳',
};

// Helper function to find the longest matching command before a position
function findCommand(text: string, pos: number): { command: string; unicode: string; start: number; end: number } | null {
    // Look backwards from position to find backslash
    let start = pos - 1;
    while (start >= 0 && /[a-zA-Z]/.test(text[start])) {
        start--;
    }
    
    if (start < 0 || text[start] !== '\\') {
        return null;
    }
    
    // Extract the command after backslash
    let end = start + 1;
    while (end < text.length && /[a-zA-Z]/.test(text[end])) {
        end++;
    }
    
    if (end === start + 1) {
        return null; // No command after backslash
    }
    
    const command = text.substring(start + 1, end);
    
    // Try to find exact match
    if (unicodeMap[command]) {
        return { 
            command, 
            unicode: unicodeMap[command],
            start: start,
            end: end
        };
    }
    
    return null;
}

// Keymap that intercepts space, enter, and tab to replace backslash commands
function agdaInputKeymap() {
    return keymap.of([
        {
            key: ' ',
            run: (view) => {
                const state = view.state;
                const selection = state.selection.main;
                const pos = selection.head;
                const text = state.doc.toString();
                
                const result = findCommand(text, pos);
                if (result) {
                    view.dispatch({
                        changes: {
                            from: result.start,
                            to: result.end,
                            insert: result.unicode + ' '
                        },
                        selection: { anchor: result.start + result.unicode.length + 1 }
                    });
                    return true;
                }
                return false;
            }
        },
        {
            key: 'Enter',
            run: (view) => {
                const state = view.state;
                const selection = state.selection.main;
                const pos = selection.head;
                const text = state.doc.toString();
                
                const result = findCommand(text, pos);
                if (result) {
                    view.dispatch({
                        changes: {
                            from: result.start,
                            to: result.end,
                            insert: result.unicode + '\n'
                        },
                        selection: { anchor: result.start + result.unicode.length + 1 }
                    });
                    return true;
                }
                return false;
            }
        },
        {
            key: 'Tab',
            run: (view) => {
                const state = view.state;
                const selection = state.selection.main;
                const pos = selection.head;
                const text = state.doc.toString();
                
                const result = findCommand(text, pos);
                if (result) {
                    view.dispatch({
                        changes: {
                            from: result.start,
                            to: result.end,
                            insert: result.unicode + '\t'
                        },
                        selection: { anchor: result.start + result.unicode.length + 1 }
                    });
                    return true;
                }
                return false;
            }
        }
    ]);
}

// Extension that provides agda-input functionality
export function agdaInput(): Extension {
    return agdaInputKeymap();
}
