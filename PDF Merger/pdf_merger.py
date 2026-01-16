import tkinter as tk
from tkinter import filedialog, messagebox
from PyPDF2 import PdfMerger
import os

class PDFMergerApp:
    def __init__(self, root):
        self.root = root
        self.root.title("PDF Merger Tool")
        self.root.geometry("750x650")
        
        self.pdf_entries = []
        self.pdf_paths = []
        self.selected_filename_var = tk.IntVar(value=0)
        
        # Main frame
        main_frame = tk.Frame(root, padx=20, pady=20)
        main_frame.pack(fill=tk.BOTH, expand=True)
        
        # Title
        title_label = tk.Label(main_frame, text="PDF Merger Tool", font=("Arial", 16, "bold"))
        title_label.pack(pady=(0, 20))
        
        # Number of PDFs input frame
        input_frame = tk.Frame(main_frame)
        input_frame.pack(fill=tk.X, pady=10)
        
        tk.Label(input_frame, text="Number of PDFs to merge:", font=("Arial", 10)).pack(side=tk.LEFT, padx=5)
        self.num_pdfs_var = tk.StringVar(value="2")
        num_pdfs_spinbox = tk.Spinbox(input_frame, from_=2, to=20, textvariable=self.num_pdfs_var, width=10)
        num_pdfs_spinbox.pack(side=tk.LEFT, padx=5)
        
        generate_btn = tk.Button(input_frame, text="Generate Input Fields", command=self.generate_input_fields, bg="#4CAF50", fg="white", font=("Arial", 9, "bold"))
        generate_btn.pack(side=tk.LEFT, padx=10)
        
        # Separator
        tk.Frame(main_frame, height=2, bg="gray").pack(fill=tk.X, pady=15)
        
        # Scrollable frame for PDF inputs
        canvas_frame = tk.Frame(main_frame)
        canvas_frame.pack(fill=tk.BOTH, expand=True, pady=10)
        
        self.canvas = tk.Canvas(canvas_frame, height=300)
        scrollbar = tk.Scrollbar(canvas_frame, orient="vertical", command=self.canvas.yview)
        self.scrollable_frame = tk.Frame(self.canvas)
        
        self.scrollable_frame.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )
        
        self.canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)
        
        self.canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # Separator
        tk.Frame(main_frame, height=2, bg="gray").pack(fill=tk.X, pady=15)
        
        # Output section
        output_frame = tk.Frame(main_frame)
        output_frame.pack(fill=tk.X, pady=10)
        
        tk.Label(output_frame, text="Output Folder:", font=("Arial", 10, "bold")).pack(side=tk.LEFT, padx=5)
        self.output_entry = tk.Entry(output_frame, width=50)
        self.output_entry.pack(side=tk.LEFT, padx=5)
        
        output_browse_btn = tk.Button(output_frame, text="Browse", command=self.browse_output)
        output_browse_btn.pack(side=tk.LEFT, padx=5)
        
        # Merge button
        merge_btn = tk.Button(main_frame, text="Merge PDFs", command=self.merge_pdfs, bg="#2196F3", fg="white", font=("Arial", 12, "bold"), pady=10)
        merge_btn.pack(pady=20, fill=tk.X)
        
        # Generate initial fields
        self.generate_input_fields()
        
    def generate_input_fields(self):
        # Clear existing entries
        for widget in self.scrollable_frame.winfo_children():
            widget.destroy()
        
        self.pdf_entries = []
        self.pdf_paths = []
        self.selected_filename_var.set(0)
        
        try:
            num_pdfs = int(self.num_pdfs_var.get())
            if num_pdfs < 2:
                messagebox.showwarning("Invalid Input", "Number of PDFs must be at least 2")
                return
        except ValueError:
            messagebox.showerror("Invalid Input", "Please enter a valid number")
            return
        
        # Create input fields for each PDF
        for i in range(num_pdfs):
            frame = tk.Frame(self.scrollable_frame, pady=5)
            frame.pack(fill=tk.X, padx=10)
            
            # Radio button for filename selection
            radio = tk.Radiobutton(frame, text="", variable=self.selected_filename_var, value=i)
            radio.pack(side=tk.LEFT, padx=5)
            
            # Label
            label = tk.Label(frame, text=f"PDF {i+1}:", width=8, anchor="w")
            label.pack(side=tk.LEFT, padx=5)
            
            # Entry
            entry = tk.Entry(frame, width=50)
            entry.pack(side=tk.LEFT, padx=5)
            self.pdf_entries.append(entry)
            self.pdf_paths.append("")
            
            # Browse button
            browse_btn = tk.Button(frame, text="Browse", command=lambda idx=i: self.browse_pdf(idx))
            browse_btn.pack(side=tk.LEFT, padx=5)
        
        # Instruction label
        instruction_frame = tk.Frame(self.scrollable_frame)
        instruction_frame.pack(fill=tk.X, pady=15, padx=10)
        tk.Label(instruction_frame, text="ℹ Select the radio button next to the PDF whose filename you want to use for the output", 
                  fg="blue", font=("Arial", 9), wraplength=650, justify=tk.LEFT).pack()
    
    def browse_pdf(self, index):
        file_path = filedialog.askopenfilename(
            title=f"Select PDF {index+1}",
            filetypes=[("PDF files", "*.pdf"), ("All files", "*.*")]
        )
        
        if file_path:
            self.pdf_entries[index].delete(0, tk.END)
            self.pdf_entries[index].insert(0, file_path)
            self.pdf_paths[index] = file_path
    
    def browse_output(self):
        folder_path = filedialog.askdirectory(title="Select Output Folder")
        
        if folder_path:
            self.output_entry.delete(0, tk.END)
            self.output_entry.insert(0, folder_path)
    
    def merge_pdfs(self):
        # Validate inputs
        if not self.output_entry.get():
            messagebox.showerror("Error", "Please select an output folder")
            return
        
        # Collect all PDF paths
        pdf_files = []
        for i, entry in enumerate(self.pdf_entries):
            path = entry.get().strip()
            if not path:
                messagebox.showerror("Error", f"Please select PDF {i+1}")
                return
            if not os.path.exists(path):
                messagebox.showerror("Error", f"PDF {i+1} does not exist:\n{path}")
                return
            pdf_files.append(path)
        
        # Get selected filename index
        selected_index = self.selected_filename_var.get()
        selected_pdf = pdf_files[selected_index]
        base_filename = os.path.splitext(os.path.basename(selected_pdf))[0]
        
        output_folder = self.output_entry.get()
        output_path = os.path.join(output_folder, f"{base_filename}.pdf")
        
        # Check if output file already exists
        if os.path.exists(output_path):
            response = messagebox.askyesno("File Exists", 
                                          f"The file '{base_filename}.pdf' already exists.\n\nDo you want to overwrite it?")
            if not response:
                return
        
        try:
            # Merge PDFs
            merger = PdfMerger()
            
            for pdf_file in pdf_files:
                merger.append(pdf_file)
            
            merger.write(output_path)
            merger.close()
            
            messagebox.showinfo("Success", 
                              f"PDFs merged successfully!\n\nOutput file:\n{output_path}\n\nTotal PDFs merged: {len(pdf_files)}")
            
        except Exception as e:
            messagebox.showerror("Error", f"Failed to merge PDFs:\n\n{str(e)}")

def main():
    root = tk.Tk()
    app = PDFMergerApp(root)
    root.mainloop()

if __name__ == "__main__":
    main()