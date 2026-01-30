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
    
    // Subscripts
    '_0': '₀',
    '_1': '₁',
    '_2': '₂',
    '_3': '₃',
    '_4': '₄',
    '_5': '₅',
    '_6': '₆',
    '_7': '₇',
    '_8': '₈',
    '_9': '₉',
    '_i': 'ᵢ',
    '_j': 'ⱼ',
    '_k': 'ₖ',
    '_n': 'ₙ',
    '_p': 'ₚ',
    '_s': 'ₛ',
    '_t': 'ₜ',
    '_x': 'ₓ',
    '_a': 'ₐ',
    '_e': 'ₑ',
    '_h': 'ₕ',
    '_o': 'ₒ',
    '_r': 'ᵣ',
    '_u': 'ᵤ',
    '_v': 'ᵥ',
    '_beta': 'ᵦ',
    '_gamma': 'ᵧ',
    '_rho': 'ᵨ',
    '_phi': 'ᵩ',
    '_chi': 'ᵪ',
    '_+': '₊',
    '_-': '₋',
    '_=': '₌',
    '_(': '₍',
    '_)': '₎',
    
    // Superscripts
    '^0': '⁰',
    '^1': '¹',
    '^2': '²',
    '^3': '³',
    '^4': '⁴',
    '^5': '⁵',
    '^6': '⁶',
    '^7': '⁷',
    '^8': '⁸',
    '^9': '⁹',
    '^i': 'ⁱ',
    '^n': 'ⁿ',
    '^+': '⁺',
    '^-': '⁻',
    '^=': '⁼',
    '^(': '⁽',
    '^)': '⁾',
    '^a': 'ᵃ',
    '^b': 'ᵇ',
    '^d': 'ᵈ',
    '^e': 'ᵉ',
    '^g': 'ᵍ',
    '^h': 'ʰ',
    '^j': 'ʲ',
    '^k': 'ᵏ',
    '^l': 'ˡ',
    '^m': 'ᵐ',
    '^o': 'ᵒ',
    '^p': 'ᵖ',
    '^r': 'ʳ',
    '^s': 'ˢ',
    '^t': 'ᵗ',
    '^u': 'ᵘ',
    '^v': 'ᵛ',
    '^w': 'ʷ',
    '^x': 'ˣ',
    '^y': 'ʸ',
    '^z': 'ᶻ',
    '^A': 'ᴬ',
    '^B': 'ᴮ',
    '^D': 'ᴰ',
    '^E': 'ᴱ',
    '^G': 'ᴳ',
    '^H': 'ᴴ',
    '^I': 'ᴵ',
    '^J': 'ᴶ',
    '^K': 'ᴷ',
    '^L': 'ᴸ',
    '^M': 'ᴹ',
    '^N': 'ᴺ',
    '^O': 'ᴼ',
    '^P': 'ᴾ',
    '^R': 'ᴿ',
    '^T': 'ᵀ',
    '^U': 'ᵁ',
    '^V': 'ⱽ',
    '^W': 'ᵂ',
    '^alpha': 'ᵅ',
    '^beta': 'ᵝ',
    '^gamma': 'ᵞ',
    '^delta': 'ᵟ',
    '^epsilon': 'ᵋ',
    '^theta': 'ᶿ',
    '^iota': 'ᶥ',
    '^phi': 'ᵠ',
    '^chi': 'ᵡ',
    
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
    // Match letters, digits, underscore, or caret (for subscripts/superscripts)
    while (start >= 0 && /[a-zA-Z0-9_^]/.test(text[start])) {
        start--;
    }
    
    if (start < 0 || text[start] !== '\\') {
        return null;
    }
    
    // Extract the command after backslash
    // For subscripts/superscripts, we match _ or ^ followed by alphanumeric
    // For regular commands, we match letters
    let end = start + 1;
    const firstChar = end < text.length ? text[end] : '';
    
    if (firstChar === '_' || firstChar === '^') {
        // For subscripts/superscripts: match _ or ^ followed by alphanumeric or special characters
        end++; // Skip the _ or ^
        const nextChar = end < text.length ? text[end] : '';
        // Check if it's a special character (single char) or alphanumeric (multi-char)
        if (/[+\-=()]/.test(nextChar)) {
            // Single special character
            end++;
        } else {
            // Alphanumeric sequence (digits, letters, or multi-letter like "beta")
            while (end < text.length && /[a-zA-Z0-9]/.test(text[end])) {
                end++;
            }
        }
    } else {
        // For regular commands: match only letters
        while (end < text.length && /[a-zA-Z]/.test(text[end])) {
            end++;
        }
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
